;;; consult-jj.el --- Browse Jujutsu changes with Consult -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))
;; Version: 0.1.0

;;; Commentary:

;; Provide commands: `consult-jj-modified-files' and `consult-jj-modified-hunks' to
;; browse changes in the Jujutsu working-copy commit with Consult previews.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'diff-mode)
(require 'vc)
(require 'consult)

(defgroup consult-jj nil
  "Browse Jujutsu changes with Consult."
  :group 'tools
  :prefix "consult-jj-")

(defcustom consult-jj-log-preview t
  "Whether `consult-jj-read-commit' previews commits and their patches."
  :type 'boolean
  :group 'consult-jj)

(defcustom consult-jj-log-function #'consult-jj-collect-commits
  "Function used by `consult-jj-log' to collect commit candidates.
The function receives the repository root and must return a list of
`consult-jj-commit' objects."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-log-visit-function #'consult-jj-default-log-visit
  "Function used by `consult-jj-log' to visit a selected commit.
The function receives the selected full commit ID.  `default-directory' is
dynamically bound to the repository root while the function runs."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-log-buffer-name "*consult-jj-show*"
  "Name of the persistent fallback buffer used to show a selected commit."
  :type 'string
  :group 'consult-jj)

(defconst consult-jj--log-preview-buffer-name "*consult-jj-log-preview*"
  "Base name of the temporary commit preview buffer.")

(require 'consult-jj-commit)
(require 'consult-jj-hunk)
(require 'consult-jj-jj)

;;;###autoload
(defun consult-jj-log ()
  "Select and visit a Jujutsu commit from the current project's log."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (commits (funcall consult-jj-log-function root)))
    (if (null commits)
        (message "No Jujutsu commits found.")
      (when-let ((selected (consult-jj-read-commit commits)))
        (funcall consult-jj-log-visit-function
                 (consult-jj-commit-commit-id selected)))))
  nil)

(defun consult-jj-read-commit (commits)
  "Read and return one structured commit from COMMITS, or nil.
COMMITS must contain `consult-jj-commit' objects.  Completion candidates show
only the first description line."
  (let ((candidates
         (cl-loop for commit in commits
                  for index from 0
                  collect (consult-jj--commit-candidate commit index)))
        preview-buffer)
    (unwind-protect
        (when candidates
          (consult--read
           candidates
           :prompt "Jujutsu commits: "
           :category 'consult-jj-commit
           :require-match t
           :sort nil
           :lookup #'consult-jj--lookup-commit
           :history '(:input consult--line-history)
           :state (when consult-jj-log-preview
                    (lambda (action commit)
                      (when (and (eq action 'preview) commit)
                        (setq preview-buffer
                              (consult-jj--preview-commit commit
                                                          preview-buffer)))))))
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer)))))

(defun consult-jj-default-log-visit (commit-id)
  "Visit COMMIT-ID using Emacs VC.
When VC does not recognize the current repository as JJ, display the commit
and its diff in a persistent Consult JJ buffer instead."
  (if (eq (vc-responsible-backend default-directory t) 'JJ)
      (vc-print-root-log 1 commit-id)
    (consult-jj--display-commit commit-id)))

;;;###autoload
(defun consult-jj-modified-files ()
  "Pick a modified file in the current project with Consult preview.
Files come from the Jujutsu working-copy commit `@'."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (files (consult-jj-collect-files root)))
    (if (null files)
        (message "No modified files found.")
      (let* ((absolute (mapcar (lambda (f) (expand-file-name f root)) files))
             (selected (consult--read
                        absolute
                        :prompt "Modified files: "
                        :category 'consult-jj-modified-file
                        :require-match t
                        :sort nil
                        :state (consult--file-preview)
                        :history 'file-name-history)))
        (when selected
          (find-file selected))))))

;;;###autoload
(defun consult-jj-modified-hunks ()
  "Pick a modified hunk in the current project with Consult preview.
Hunks come from the Jujutsu working-copy commit `@'."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (hunks (consult-jj-collect-hunks root))
         (candidates (mapcar (lambda (hunk) (consult-jj--hunk-candidate hunk root))
                             hunks)))
    (if (null candidates)
        (message "No modified hunks found.")
      (when-let ((selected
                  (consult--read
                   candidates
                   :prompt "Modified hunks: "
                   :category 'consult-jj-modified-hunk
                   :require-match t
                   :sort nil
                   :lookup #'consult-jj--lookup-hunk
                   :history '(:input consult--line-history)
                   :state (consult-jj--hunk-state candidates))))
        (consult-jj-visit-hunk selected root)))))

(defun consult-jj--root ()
  "Return the current project root, or signal a `user-error'."
  (let ((project (project-current nil)))
    (unless project
      (user-error "consult-jj: No project found for %s" default-directory))
    (expand-file-name (project-root project))))

(defun consult-jj--commit-candidate (commit index)
  "Build a completion candidate for COMMIT disambiguated by INDEX."
  (let* ((description (consult-jj-commit-description commit))
         (first-line (car (split-string description "\n")))
         (display (if (string-empty-p (or first-line ""))
                      "(no description set)"
                    first-line))
         (candidate (consult--tofu-append display index)))
    (add-text-properties 0 1 (list 'consult-jj-commit commit) candidate)
    candidate))

(defun consult-jj--lookup-commit (selected candidates &rest _)
  "Return the commit object for SELECTED from CANDIDATES."
  (when-let ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-commit candidate)))

(defun consult-jj--preview-commit (commit buffer)
  "Render COMMIT into reusable preview BUFFER and return the buffer."
  (unless (buffer-live-p buffer)
    (setq buffer (generate-new-buffer consult-jj--log-preview-buffer-name)))
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (consult-jj-jj--run default-directory "show" "--git"
                                  (consult-jj-commit-commit-id commit))))
    (diff-mode)
    (goto-char (point-min)))
  (display-buffer buffer)
  buffer)

(defun consult-jj--display-commit (commit-id)
  "Render COMMIT-ID into the persistent fallback display buffer."
  (let ((root default-directory)
        (buffer (get-buffer-create consult-jj-log-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory root)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (consult-jj-jj--run root "show" "--git" commit-id)))
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

(defun consult-jj--hunk-candidate (hunk root)
  "Build a `consult-location' candidate for HUNK under ROOT.
The candidate carries HUNK in a text property so lookup returns the
object rather than its display string.  When the worktree
file is unavailable (for example, after deletion), return a display-only
candidate which says that preview is unavailable."
  (let* ((path (consult-jj-hunk-preview-path hunk))
         (abs (and path (expand-file-name path root)))
         (buf (and abs (or (get-file-buffer abs)
                           (and (file-readable-p abs) (find-file-noselect abs t)))))
         (line (max 1 (or (consult-jj-hunk-first-changed-line hunk) 1)))
         (context (consult-jj-hunk-context hunk))
         (rel (if abs (file-relative-name abs root) "<unknown path>"))
         (snippet (if buf
                      (with-current-buffer buf
                        (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-min))
                            (forward-line (1- line))
                            (buffer-substring (pos-bol) (pos-eol)))))
                    (propertize "preview unavailable" 'face 'shadow)))
         (suffix (if (string-empty-p context)
                     snippet
                   (concat (propertize context 'face 'shadow)
                           (if (string-empty-p snippet) "" "  ")
                           snippet)))
         (display (consult--format-file-line-match rel line suffix)))
    (if buf
        (let ((pos (with-current-buffer buf
                     (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char (point-min))
                         (forward-line (1- line))
                         (pos-bol))))))
          (consult--location-candidate
           display (cons buf pos) line line 'consult-jj-hunk hunk))
      (add-text-properties 0 1 (list 'consult-jj-hunk hunk) display)
      display)))

(defun consult-jj--lookup-hunk (selected candidates &rest _)
  "Return the hunk object for SELECTED from CANDIDATES."
  (when-let ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-hunk candidate)))

(defun consult-jj--hunk-state (candidates)
  "Return preview state for hunk CANDIDATES."
  (let ((location-state (consult--location-state candidates)))
    (lambda (action candidate)
      (cond
       ((and (eq action 'return) (consult-jj-hunk-p candidate)))
       ((consult-jj-hunk-p candidate)
        (funcall location-state action
                 (consult-jj--hunk-location candidate candidates)))
       (t (funcall location-state action candidate))))))

(defun consult-jj--hunk-location (hunk candidates)
  "Return HUNK's preview marker from CANDIDATES, or nil."
  (when-let ((candidate
              (cl-find-if
               (lambda (item)
                 (eq (get-text-property 0 'consult-jj-hunk item) hunk))
               candidates)))
    (car (consult--get-location candidate))))

(defun consult-jj-visit-hunk (hunk &optional root)
  "Visit HUNK's worktree location under ROOT when it is available.
When ROOT is nil, use the root recorded on HUNK or `default-directory'."
  (setq root (or root (consult-jj-hunk-root hunk) default-directory))
  (let* ((path (consult-jj-hunk-preview-path hunk))
         (absolute (and path (expand-file-name path root))))
    (if (and absolute (file-readable-p absolute))
        (progn
          (find-file absolute)
          (widen)
          (goto-char (point-min))
          (forward-line (1- (max 1 (consult-jj-hunk-first-changed-line hunk)))))
      (message "consult-jj: `%s' is not available in the worktree"
               (or path "unknown path")))))

(provide 'consult-jj)
;;; consult-jj.el ends here
