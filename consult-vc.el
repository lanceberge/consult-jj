;;; consult-vc.el --- Browse and act on VC hunks with Consult and Embark -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))
;; Version: 0.1.0

;;; Commentary:

;; `consult-vc-modified-files' and `consult-vc-modified-hunks' browse the
;; current version-control changes with Consult, previewing each file or

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'consult)
(require 'consult-vc-hunk)
(require 'consult-vc-provider)

;;;###autoload
(defun consult-vc-modified-files ()
  "Pick a modified file in the current project with Consult preview."
  (interactive)
  (let* ((root (consult-vc--root))
         (default-directory root)
         (provider (consult-vc-provider-resolve root))
         (files (funcall (consult-vc-provider-def-collect-files provider) root)))
    (if (null files)
        (message "No modified, new, or staged files found.")
      (let* ((absolute (mapcar (lambda (f) (expand-file-name f root)) files))
             (selected (consult--read
                        absolute
                        :prompt "Modified files: "
                        :category 'file
                        :require-match t
                        :sort nil
                        :state (consult--file-preview)
                        :history 'file-name-history)))
        (when selected
          (find-file selected))))))

;;;###autoload
(defun consult-vc-modified-hunks ()
  "Pick a modified hunk in the current project with Consult preview."
  (interactive)
  (let* ((root (consult-vc--root))
         (default-directory root)
         (provider (consult-vc-provider-resolve root))
         (hunks (funcall (consult-vc-provider-def-collect-hunks provider) root))
         (candidates (mapcar (lambda (hunk) (consult-vc--hunk-candidate hunk root))
                             hunks)))
    (if (null candidates)
        (message "No modified hunks found.")
      (when-let ((selected
                  (consult--read
                   candidates
                   :prompt "Modified hunks: "
                   :category 'consult-location
                   :require-match t
                   :sort nil
                   :lookup #'consult-vc--lookup-hunk
                   :history '(:input consult--line-history)
                   :state (consult-vc--hunk-state candidates))))
        (consult-vc--visit-hunk selected root)))))

(defun consult-vc--root ()
  "Return the current project root, or `user-error'"
  (let ((project (project-current nil)))
    (unless project
      (user-error "consult-vc: no project found for %s" default-directory))
    (expand-file-name (project-root project))))

(defun consult-vc--hunk-candidate (hunk root)
  "Build a `consult-location' candidate for HUNK under ROOT.
The candidate carries HUNK in a text property so lookup returns the
provider-neutral object rather than its display string."
  (let* ((path (consult-vc-hunk-preview-path hunk))
         (abs (and path (expand-file-name path root)))
         (buf (and abs (or (get-file-buffer abs)
                           (and (file-readable-p abs) (find-file-noselect abs t)))))
         (line (max 1 (or (consult-vc-hunk-first-changed-line hunk) 1)))
         (context (consult-vc-hunk-context hunk))
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
           display (cons buf pos) line line 'consult-vc-hunk hunk))
      (add-text-properties 0 1 (list 'consult-vc-hunk hunk) display)
      display)))

(defun consult-vc--lookup-hunk (selected candidates &rest _)
  "Return the hunk object for SELECTED from CANDIDATES."
  (when-let ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-vc-hunk candidate)))

(defun consult-vc--hunk-state (candidates)
  "Return preview state for hunk CANDIDATES."
  (let ((location-state (consult--location-state candidates)))
    (lambda (action candidate)
      (cond
       ((and (eq action 'return) (consult-vc-hunk-p candidate)))
       ((consult-vc-hunk-p candidate)
        (funcall location-state action
                 (consult-vc--hunk-location candidate candidates)))
       (t (funcall location-state action candidate))))))

(defun consult-vc--hunk-location (hunk candidates)
  "Return HUNK's preview marker from CANDIDATES, or nil."
  (when-let ((candidate
              (cl-find-if
               (lambda (item)
                 (eq (get-text-property 0 'consult-vc-hunk item) hunk))
               candidates)))
    (car (consult--get-location candidate))))

(defun consult-vc--visit-hunk (hunk root)
  "Visit HUNK's worktree location under ROOT when it is available."
  (let* ((path (consult-vc-hunk-preview-path hunk))
         (absolute (and path (expand-file-name path root))))
    (if (and absolute (file-readable-p absolute))
        (progn
          (find-file absolute)
          (widen)
          (goto-char (point-min))
          (forward-line (1- (max 1 (consult-vc-hunk-first-changed-line hunk)))))
      (message "consult-vc: `%s' is not available in the worktree"
               (or path "unknown path")))))

(provide 'consult-vc)
;;; consult-vc.el ends here
