;;; consult-jj.el --- Browse Jujutsu changes with Consult -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0") (transient "0.3.0"))
;; Version: 0.1.0

;;; Commentary:

;; Provide commands: `consult-jj-modified-files' and `consult-jj-modified-hunks' to
;; browse changes in the Jujutsu working-copy commit with Consult previews.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'diff-mode)
(require 'consult)
(require 'transient)

(defgroup consult-jj nil
  "Browse Jujutsu changes with Consult."
  :group 'tools
  :prefix "consult-jj-")

(defcustom consult-jj-log-preview-style 'diff
  "Preview style used by `consult-jj-read-commit'.
The `diff' style shows the selected commit and its patch.  The `none' style
disables preview."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "None" none))
  :group 'consult-jj)

(make-obsolete-variable 'consult-jj-log-preview
                        'consult-jj-log-preview-style "0.2.0")

(defcustom consult-jj-modified-files-preview-style 'diff
  "Preview style used by `consult-jj-modified-files'.
The `diff' style shows all modified hunks in the selected file.  The `file'
style visits the selected worktree file temporarily.  The `none' style
disables preview."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "Visit" visit)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-modified-hunks-preview-style 'diff
  "Preview style used by `consult-jj-modified-hunks'.
The `diff' style shows the selected modified hunk.  The `file' style visits
the hunk's worktree location temporarily.  The `none' style disables preview."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "Visit" visit)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-squash-immutable-policy 'ask
  "How `consult-jj-squash' handles an immutable commit.
The value `ask' requests confirmation, `ignore' allows the rewrite without
confirmation, and `refuse' cancels the squash without confirmation."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Ignore" ignore)
                 (const :tag "Refuse" refuse))
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
  "Name of the persistent buffer used to show a selected commit."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-description-function nil
  "Function used to transform a commit description before a mutation.
The function receives the entered or supplied description.  When this option
is nil, or when the function returns nil, the description is unchanged."
  :type '(choice (const :tag "Unchanged" nil) function)
  :group 'consult-jj)

(defconst consult-jj--log-preview-buffer-name "*consult-jj-log-preview*"
  "Base name of the temporary commit preview buffer.")

(defconst consult-jj--diff-preview-buffer-name "*consult-jj-diff-preview*"
  "Base name of the temporary modified-change preview buffer.")

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

(defun consult-jj-read-commit (commits &optional prompt)
  "Read and return one structured commit from COMMITS, or nil.
COMMITS must contain `consult-jj-commit' objects.  Completion candidates show
only the first description line.  PROMPT defaults to `Jujutsu commits: '."
  (let ((candidates
         (cl-loop for commit in commits
                  for index from 0
                  collect (consult-jj--commit-candidate commit index)))
        (state (consult-jj--log-preview-state)))
    (when candidates
      (consult--read
       candidates
       :prompt (or prompt "Jujutsu commits: ")
       :category 'consult-jj-commit
       :require-match t
       :sort nil
       :lookup #'consult-jj--lookup-commit
       :history '(:input consult--line-history)
       :state state))))

(defun consult-jj-default-log-visit (commit-id)
  "Display COMMIT-ID and its diff in a persistent Consult JJ buffer."
  (consult-jj--display-commit commit-id))

;;;###autoload
(defun consult-jj-rebase (&optional source destination root)
  "Rebase SOURCE relative to DESTINATION after choosing its placement.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
When either is nil, read it from the structured Jujutsu log under ROOT.  Then
display the placement transient."
  (interactive)
  (when-let ((targets
              (consult-jj--read-rebase-targets
               source destination root "Rebase destination: ")))
    (apply #'consult-jj--rebase-placement targets))
  nil)

;;;###autoload
(defun consult-jj-rebase-onto (&optional source destination root)
  "Rebase SOURCE and its descendants onto DESTINATION under ROOT.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
Read either omitted value from the structured Jujutsu log."
  (interactive
   (or (transient-scope 'consult-jj--rebase-placement)
       (list nil nil nil)))
  (consult-jj--rebase-with-placement source destination 'onto root))

;;;###autoload
(defun consult-jj-rebase-after (&optional source destination root)
  "Rebase SOURCE and its descendants after DESTINATION under ROOT.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
Read either omitted value from the structured Jujutsu log."
  (interactive
   (or (transient-scope 'consult-jj--rebase-placement)
       (list nil nil nil)))
  (consult-jj--rebase-with-placement source destination 'after root))

;;;###autoload
(defun consult-jj-rebase-before (&optional source destination root)
  "Rebase SOURCE and its descendants before DESTINATION under ROOT.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
Read either omitted value from the structured Jujutsu log."
  (interactive
   (or (transient-scope 'consult-jj--rebase-placement)
       (list nil nil nil)))
  (consult-jj--rebase-with-placement source destination 'before root))

(transient-define-prefix consult-jj--rebase-placement
  (source destination root)
  "Choose how to place SOURCE relative to DESTINATION under ROOT."
  ["Placement"
   ("a" "After" consult-jj-rebase-after)
   ("b" "Before" consult-jj-rebase-before)
   ("o" "Onto" consult-jj-rebase-onto)]
  (interactive (list nil nil nil))
  (transient-setup 'consult-jj--rebase-placement nil nil
                   :scope (list source destination root)))

;;;###autoload
(defun consult-jj-modified-files ()
  "Pick a modified file in the current project with Consult preview.
Files come from the Jujutsu working-copy commit `@'."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (groups
          (if (eq consult-jj-modified-files-preview-style 'diff)
              (consult-jj--group-hunks-by-file
               (consult-jj-collect-hunks root) root)
            (mapcar (lambda (file)
                      (list (expand-file-name file root)))
                    (consult-jj-collect-files root)))))
    (if (null groups)
        (message "No modified files found.")
      (let* ((absolute (mapcar #'car groups))
             (selected (consult--read
                        absolute
                        :prompt "Modified files: "
                        :category 'consult-jj-modified-file
                        :require-match t
                        :sort nil
                        :state (consult-jj--modified-file-preview-state groups)
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
                   :state (consult-jj--modified-hunk-preview-state candidates))))
        (consult-jj-visit-hunk selected root)))))

;;;###autoload
(defun consult-jj-diff (targets &optional root)
  "Display a persistent Git-format diff for modified TARGETS under ROOT.
TARGETS must be a homogeneous target set of file names or `consult-jj-hunk'
objects.  Hunk targets are rendered from their captured diff snapshot; file
targets are read from the current working-copy commit."
  (interactive
   (let ((root (consult-jj--root)))
     (list (consult-jj-collect-hunks root) root)))
  (when (null targets)
    (user-error "consult-jj: Diff requires at least one target"))
  (let ((diff
         (cond
          ((cl-every #'consult-jj-hunk-p targets)
           (setq root (or root (consult-jj-hunk-root (car targets))))
           (consult-jj-hunk->diff targets))
          ((cl-every #'stringp targets)
           (setq root (or root
                          (locate-dominating-file (car targets) ".jj")))
           (unless root
             (user-error "consult-jj: No Jujutsu repository found for `%s'"
                         (car targets)))
           (consult-jj-jj--diff-files targets root))
          (t
           (user-error "consult-jj: Diff targets must all have the same kind")))))
    (consult-jj--display-diff diff consult-jj-hunk-diff-buffer-name root)))

;;;###autoload
(defun consult-jj-restore (targets &optional root)
  "Restore modified-file or modified-hunk TARGETS in Jujutsu.
TARGETS must be a homogeneous target set of file names or `consult-jj-hunk'
objects.  ROOT is the repository root.  Interactively, restore all modified
hunks in the current project."
  (interactive
   (let ((root (expand-file-name (project-root (project-current t)))))
     (list (consult-jj-collect-hunks root) root)))
  (cond
   ((null targets)
    (user-error "consult-jj: restore requires at least one target"))
   ((cl-every #'consult-jj-hunk-p targets)
    (consult-jj-jj--restore-hunks targets root))
   ((cl-every #'stringp targets)
    (consult-jj-jj--restore-files targets root))
   (t
    (user-error "consult-jj: restore targets must all have the same kind")))
  nil)

;;;###autoload
(defun consult-jj-squash (targets &optional destination root)
  "Squash modified-file or modified-hunk TARGETS into DESTINATION in Jujutsu.
DESTINATION is a Jujutsu revset.  When it is nil, read a destination from the
repository log.  ROOT overrides the repository root inferred from the targets.
`consult-jj-squash-immutable-policy' controls immutable rewrites."
  (interactive
   (let ((root (expand-file-name (project-root (project-current t)))))
     (list (consult-jj-collect-hunks root) nil root)))
  (when (null targets)
    (user-error "consult-jj: Squash requires at least one target"))
  (unless (memq consult-jj-squash-immutable-policy '(ask ignore refuse))
    (user-error "consult-jj: Invalid immutable policy `%s'"
                consult-jj-squash-immutable-policy))
  (let ((kind (cond
               ((cl-every #'consult-jj-hunk-p targets) 'hunk)
               ((cl-every #'stringp targets) 'file)
               (t (user-error
                   "consult-jj: Squash targets must all have the same kind")))))
    (setq root
          (or root
              (if (eq kind 'hunk)
                  (consult-jj-hunk-root (car targets))
                (locate-dominating-file (car targets) ".jj"))))
    (unless root
      (user-error "consult-jj: No Jujutsu repository found for squash targets"))
    (setq destination (or destination
                          (consult-jj--read-squash-destination root)))
    (when destination
      (let ((result
             (cond
              ((and (eq kind 'hunk)
                    (eq consult-jj-squash-immutable-policy 'ignore))
               (consult-jj-jj--squash-hunks targets destination root t))
              ((eq kind 'hunk)
               (consult-jj-jj--squash-hunks targets destination root))
              ((eq consult-jj-squash-immutable-policy 'ignore)
               (consult-jj-jj--squash-files targets destination root t))
              (t
               (consult-jj-jj--squash-files targets destination root)))))
        (when (and (eq consult-jj-squash-immutable-policy 'ask)
                   (eq result 'immutable)
                   (y-or-n-p "Commit is immutable, ignore: "))
          (setq result
                (if (eq kind 'hunk)
                    (consult-jj-jj--squash-hunks
                     targets destination root t)
                  (consult-jj-jj--squash-files
                   targets destination root t))))
        (unless (eq result 'immutable)
          (if (and (integerp result) (> result 0))
              (message "squashed changes into %s with %d conflicts"
                       destination result)
            (message "squashed changes into %s" destination))))))
  nil)

;;;###autoload
(defun consult-jj-split (targets &optional description)
  "Split modified-file or modified-hunk TARGETS into a new child commit.
TARGETS must be a homogeneous target set of file names or `consult-jj-hunk'
objects.  The selected changes remain in the original commit and receive
DESCRIPTION; the remaining changes move into a new child commit.  When
DESCRIPTION is nil, read it from the minibuffer."
  (interactive
   (let ((root (consult-jj--root)))
     (list (consult-jj-collect-hunks root) nil)))
  (when (null targets)
    (user-error "consult-jj: Split requires at least one target"))
  (let* ((root (consult-jj--root))
         (description (or description (read-string "Description: " "")))
         (final-description
          (or (and consult-jj-description-function
                   (funcall consult-jj-description-function description))
              description)))
    (cond
     ((cl-every #'consult-jj-hunk-p targets)
      (consult-jj-jj--split-hunks targets final-description root))
     ((cl-every #'stringp targets)
      (consult-jj-jj--split-files targets final-description root))
     (t
      (user-error "consult-jj: Split targets must all have the same kind"))))
  nil)

(defun consult-jj--read-squash-destination (root)
  "Read a squash destination from the Jujutsu log under ROOT."
  (let* ((default-directory root)
         (commits
          (cl-remove-if #'consult-jj-commit-current-p
                        (funcall consult-jj-log-function root)))
         (selected (consult-jj-read-commit commits)))
    (when selected
      (consult-jj-commit-commit-id selected))))

(defun consult-jj--rebase-with-placement (source destination placement root)
  "Rebase SOURCE at DESTINATION using PLACEMENT under ROOT."
  (when-let ((targets
              (consult-jj--read-rebase-targets
               source destination root (format "Rebase %s: " placement))))
    (pcase-let ((`(,source ,destination ,root) targets))
      (consult-jj-jj--rebase
       (consult-jj--commit-id source)
       (consult-jj--commit-id destination)
       placement root)))
  nil)

(defun consult-jj--read-rebase-targets
    (source destination root destination-prompt)
  "Return SOURCE, DESTINATION, and ROOT for a rebase, reading omitted values.
DESTINATION-PROMPT labels the structured-log destination selection."
  (setq root (or root (consult-jj--root)))
  (let* ((default-directory root)
         (commits (and (or (null source) (null destination))
                       (funcall consult-jj-log-function root))))
    (setq source (or source
                     (consult-jj-read-commit commits "Commit to rebase: ")))
    (when source
      (setq destination
            (or destination
                (consult-jj-read-commit commits destination-prompt)))
      (when destination
        (list source destination root)))))

(defun consult-jj--commit-id (commit)
  "Return the commit ID represented by COMMIT."
  (if (consult-jj-commit-p commit)
      (consult-jj-commit-commit-id commit)
    commit))

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

(defun consult-jj--log-preview-state ()
  "Return the preview state selected by `consult-jj-log-preview-style'."
  (pcase consult-jj-log-preview-style
    ('diff
     (consult-jj--diff-preview-state
      (lambda (commit)
        (consult-jj--commit-diff (consult-jj-commit-commit-id commit)
                                 default-directory))
      consult-jj--log-preview-buffer-name))
    ('none nil)
    (style (user-error "consult-jj: Invalid log preview style `%s'" style))))

(defun consult-jj--modified-file-preview-state (groups)
  "Return the configured modified-file preview state for GROUPS.
GROUPS is an alist of absolute file names to captured hunks."
  (pcase consult-jj-modified-files-preview-style
    ('diff
     (consult-jj--diff-preview-state
      (lambda (file)
        (consult-jj-hunk->diff (cdr (assoc-string file groups))))
      consult-jj--diff-preview-buffer-name))
    ('visit (consult--file-preview))
    ('none nil)
    (style
     (user-error "consult-jj: Invalid modified-file preview style `%s'" style))))

(defun consult-jj--modified-hunk-preview-state (candidates)
  "Return the configured modified-hunk preview state for CANDIDATES."
  (pcase consult-jj-modified-hunks-preview-style
    ('diff
     (consult-jj--diff-preview-state
      (lambda (hunk) (consult-jj-hunk->diff (list hunk)))
      consult-jj--diff-preview-buffer-name))
    ('visit (consult-jj--hunk-state candidates))
    ('none nil)
    (style
     (user-error "consult-jj: Invalid modified-hunk preview style `%s'" style))))

(defun consult-jj--diff-preview-state (render buffer-name)
  "Return a transient diff preview state using RENDER and BUFFER-NAME.
RENDER receives the typed candidate and returns Git-format diff text."
  (let ((preview (consult--buffer-preview))
        buffer)
    (lambda (action candidate)
      (when (and (eq action 'preview) candidate)
        (unless (buffer-live-p buffer)
          (setq buffer (generate-new-buffer buffer-name)))
        (consult-jj--render-diff (funcall render candidate) buffer))
      (funcall preview action
               (and (eq action 'preview) candidate buffer))
      (when (and (memq action '(exit return)) (buffer-live-p buffer))
        (kill-buffer buffer)))))

(defun consult-jj--display-diff (diff buffer-name root)
  "Display DIFF persistently in BUFFER-NAME with directory ROOT."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq default-directory root))
    (consult-jj--render-diff diff buffer)
    (display-buffer buffer)
    buffer))

(defun consult-jj--render-diff (diff buffer)
  "Render DIFF in BUFFER and return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert diff))
    (diff-mode)
    (goto-char (point-min)))
  buffer)

(defun consult-jj--group-hunks-by-file (hunks root)
  "Group HUNKS by absolute preview path under ROOT, preserving order."
  (let (groups)
    (dolist (hunk hunks)
      (let* ((path (consult-jj-hunk-preview-path hunk))
             (absolute (and path (expand-file-name path root)))
             (group (and absolute (assoc-string absolute groups))))
        (when absolute
          (if group
              (setcdr group (append (cdr group) (list hunk)))
            (setq groups (append groups (list (list absolute hunk))))))))
    groups))

(defun consult-jj--display-commit (commit-id)
  "Render COMMIT-ID into the persistent log display buffer."
  (consult-jj--display-diff
   (consult-jj--commit-diff commit-id default-directory)
   consult-jj-log-buffer-name
   default-directory))

(defun consult-jj--commit-diff (commit-id root)
  "Return the Git-format diff presentation of COMMIT-ID under ROOT."
  (consult-jj-jj--run root "show" "--git" commit-id))

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
