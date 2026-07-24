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

(defcustom consult-jj-description-style 'prompt
  "Interaction used to edit a commit description.
The `prompt' style edits in the minibuffer.  The `commit' style uses a
dedicated Consult JJ editing buffer."
  :type '(choice (const :tag "Minibuffer prompt" prompt)
                 (const :tag "Commit editing buffer" commit))
  :group 'consult-jj)

(defcustom consult-jj-description-buffer-name "*consult-jj-description*"
  "Name of the buffer used by the `commit' description style."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-commit-modified-hook nil
  "Hook run after Consult JJ successfully modifies commit history."
  :type 'hook
  :group 'consult-jj)

(defcustom consult-jj-candidate-session-refreshed-hook nil
  "Hook run after a live Consult JJ session replaces its candidates.
The hook runs with no arguments in the session's minibuffer buffer.  Optional
integration extensions can use it to clear package-specific selection state."
  :type 'hook
  :group 'consult-jj)

(defconst consult-jj--log-preview-buffer-name "*consult-jj-log-preview*"
  "Base name of the temporary commit preview buffer.")

(defconst consult-jj--diff-preview-buffer-name "*consult-jj-diff-preview*"
  "Base name of the temporary modified-change preview buffer.")

(cl-defstruct (consult-jj--candidate-session
               (:constructor consult-jj--candidate-session-create)
               (:copier nil))
  "One active Consult JJ candidate session."
  root view buffer replace)

(defvar consult-jj--candidate-sessions nil
  "Currently active Consult JJ candidate sessions.")

(defvar consult-jj--commit-modified-root nil
  "Repository root for the current commit-modification notification.")

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
      (when-let ((selected (consult-jj--read-commit commits nil root)))
        (funcall consult-jj-log-visit-function
                 (consult-jj-commit-commit-id selected)))))
  nil)

(defun consult-jj-read-commit (commits &optional prompt default)
  "Read and return one structured commit from COMMITS, or nil.
COMMITS must contain `consult-jj-commit' objects.  Completion candidates show
only the first description line.  PROMPT defaults to `Jujutsu commits: '.
DEFAULT, when non-nil, is the commit offered as the default candidate."
  (consult-jj--read-commit commits prompt nil default))

(defun consult-jj--read-commit (commits prompt &optional live-root default)
  "Read one of COMMITS using PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable log session there.
DEFAULT, when non-nil, is the commit offered as the default candidate."
  (let* ((candidates (consult-jj--commit-candidates commits))
         (default-candidate
          (and
           default
           (cl-find-if
            (lambda (candidate)
              (equal
               (consult-jj-commit-commit-id
                (get-text-property 0 'consult-jj-commit candidate))
               (consult-jj-commit-commit-id default)))
            candidates)))
        (state (consult-jj--log-preview-state)))
    (when candidates
      (consult--read
       (if live-root
           (consult-jj--live-candidate-collection
            candidates live-root 'log)
         candidates)
       :prompt (or prompt "Jujutsu commits: ")
       :category 'consult-jj-commit
       :require-match t
       :sort nil
       :lookup #'consult-jj--lookup-commit
       :default default-candidate
       :history '(:input consult--line-history)
       :state state))))

(defun consult-jj-default-log-visit (commit-id)
  "Display COMMIT-ID and its diff in a persistent Consult JJ buffer."
  (consult-jj--display-commit commit-id))

;;;###autoload
(defun consult-jj-commit-duplicate
    (&optional source destination placement root)
  "Duplicate SOURCE, preserving its existing parents by default.
SOURCE and DESTINATION may be structured commit candidates or revision
strings.  When DESTINATION is non-nil and PLACEMENT is nil, duplicate onto
DESTINATION.  PLACEMENT may instead be `onto', `after', or `before'.

Interactively, read SOURCE and duplicate it without a placement override.  With
a prefix argument, read SOURCE and display the shared placement transient; the
selected placement command then reads DESTINATION.  ROOT is the repository
root."
  (interactive
   (list nil nil (and current-prefix-arg 'choose)))
  (setq root (or root (consult-jj--root)))
  (let ((default-directory root))
    (setq source
          (or source
              (consult-jj-read-commit
               (funcall consult-jj-log-function root)
               "Commit to duplicate: "))))
  (when source
    (cond
     ((eq placement 'choose)
      (consult-jj--placement
       #'consult-jj--duplicate-at-placement
       (list source destination root)))
     ((or destination placement)
      (consult-jj--duplicate-at-placement
       (or placement 'onto) source destination root))
     (t
      (consult-jj--perform-duplicate source nil nil root))))
  nil)

;;;###autoload
(defun consult-jj-commit-duplicate-onto
    (&optional source destination root)
  "Duplicate SOURCE onto DESTINATION under ROOT.
Read either omitted commit from the structured Jujutsu log."
  (interactive)
  (consult-jj--duplicate-with-placement
   source destination 'onto root))

;;;###autoload
(defun consult-jj-commit-duplicate-after
    (&optional source destination root)
  "Duplicate SOURCE after DESTINATION under ROOT.
Read either omitted commit from the structured Jujutsu log."
  (interactive)
  (consult-jj--duplicate-with-placement
   source destination 'after root))

;;;###autoload
(defun consult-jj-commit-duplicate-before
    (&optional source destination root)
  "Duplicate SOURCE before DESTINATION under ROOT.
Read either omitted commit from the structured Jujutsu log."
  (interactive)
  (consult-jj--duplicate-with-placement
   source destination 'before root))

;;;###autoload
(defun consult-jj-commit-abandon (&optional commit confirmed root)
  "Abandon one COMMIT and rebase its descendants.
COMMIT may be a structured commit candidate or revision string.  When
CONFIRMED is non-nil, do not request confirmation before mutating.  ROOT is
the repository root."
  (interactive)
  (setq root (or root (consult-jj--root)))
  (let ((default-directory root))
    (setq commit
          (or commit
              (consult-jj-read-commit
               (funcall consult-jj-log-function root)
               "Commit to abandon: "))))
  (when commit
    (let ((commit-id
           (consult-jj-jj--resolve-single-revision
            (consult-jj--commit-id commit) root)))
      (when (or confirmed
                (y-or-n-p (format "Abandon commit %s? " commit-id)))
        (setq root (file-name-as-directory (expand-file-name root)))
        (let ((consult-jj--commit-modified-root root))
          (consult-jj--complete-abandon commit-id root)))))
  nil)

;;;###autoload
(defun consult-jj-commit-describe (&optional commit description)
  "Replace COMMIT's description with DESCRIPTION.
COMMIT may be a structured commit candidate or revision string.  Lisp callers
that provide COMMIT and DESCRIPTION do not prompt or invoke an editor."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root))
    (setq commit
          (or commit
              (consult-jj-read-commit
               (funcall consult-jj-log-function root)
               "Commit to describe: ")))
    (when commit
      (let* ((current-description
              (consult-jj--commit-description commit root))
             (entered-description
              (or description
                  (consult-jj--read-description current-description)))
             (final-description
              (and
               (stringp entered-description)
               (or (and consult-jj-description-function
                        (funcall consult-jj-description-function
                                 entered-description))
                   entered-description))))
        (when (and (stringp final-description)
                   (not (equal final-description current-description)))
          (let ((consult-jj--commit-modified-root
                 (file-name-as-directory (expand-file-name root))))
            (consult-jj-jj--describe
             (consult-jj--commit-id commit) final-description root)
            (run-hooks 'consult-jj-commit-modified-hook))))))
  nil)

(defun consult-jj--read-description (initial)
  "Read a commit description initialized with INITIAL, or return nil."
  (pcase consult-jj-description-style
    ('prompt (read-from-minibuffer "Description: " initial))
    ('commit (consult-jj--edit-description initial))
    (style
     (user-error "consult-jj: Invalid description style `%s'" style))))

(defun consult-jj--complete-abandon (commit-id root)
  "Abandon COMMIT-ID under ROOT, asking before an immutable rewrite."
  (let ((result (consult-jj-jj--abandon commit-id root)))
    (when (and (eq result 'immutable)
               (y-or-n-p "Commit is immutable, ignore: "))
      (setq result (consult-jj-jj--abandon commit-id root t)))
    (unless (eq result 'immutable)
      (run-hooks 'consult-jj-commit-modified-hook))))

(defvar consult-jj-description-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'consult-jj-description-finish)
    (define-key map (kbd "C-c C-k") #'consult-jj-description-cancel)
    map)
  "Keymap for `consult-jj-description-mode'.")

(define-derived-mode consult-jj-description-mode text-mode
  "Consult-JJ-Description"
  "Major mode for editing a Jujutsu commit description."
  (setq-local header-line-format
              (substitute-command-keys
               "Finish with \\[consult-jj-description-finish]; \
cancel with \\[consult-jj-description-cancel]")))

(defvar-local consult-jj--description-finished-p nil
  "Non-nil after finishing the current description edit.")

(defun consult-jj-description-finish ()
  "Finish the current Consult JJ commit-description edit."
  (interactive)
  (unless (derived-mode-p 'consult-jj-description-mode)
    (user-error "Not editing a Consult JJ commit description"))
  (setq consult-jj--description-finished-p t)
  (exit-recursive-edit))

(defun consult-jj-description-cancel ()
  "Cancel the current Consult JJ commit-description edit."
  (interactive)
  (unless (derived-mode-p 'consult-jj-description-mode)
    (user-error "Not editing a Consult JJ commit description"))
  (abort-recursive-edit))

(defun consult-jj--edit-description (initial)
  "Edit a commit description initialized with INITIAL, or return nil."
  (let ((buffer (generate-new-buffer consult-jj-description-buffer-name))
        (configuration (current-window-configuration))
        result)
    (unwind-protect
        (condition-case nil
            (progn
              (with-current-buffer buffer
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert initial))
                (consult-jj-description-mode)
                (goto-char (point-min)))
              (pop-to-buffer buffer)
              (recursive-edit)
              (with-current-buffer buffer
                (when consult-jj--description-finished-p
                  (setq result (buffer-substring-no-properties
                                (point-min) (point-max))))))
          (quit nil))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    result))

;;;###autoload
(defun consult-jj-new-here (&optional anchor description no-edit root)
  "Create a new empty child commit of ANCHOR.
Interactively, ANCHOR is the working-copy commit `@'.  With a prefix argument,
prompt for DESCRIPTION.  When NO-EDIT is non-nil, do not make the new commit
the working-copy commit."
  (interactive
   (list "@" (when current-prefix-arg
               (read-string "Description: " ""))))
  (consult-jj--new-with-placement
   (or anchor "@") 'onto description no-edit root))

;;;###autoload
(defun consult-jj-new (&optional anchor description no-edit root)
  "Create a new empty commit relative to ANCHOR using chosen placement.
ANCHOR may be a structured commit candidate or revision string.  When it is
nil, read it from the structured Jujutsu log under ROOT.  DESCRIPTION is the
new commit description.  When NO-EDIT is non-nil, do not make the new commit
the working-copy commit."
  (interactive
   (list nil (when current-prefix-arg
               (read-string "Description: " ""))))
  (setq root (or root (consult-jj--root)))
  (let ((default-directory root))
    (setq anchor
          (or anchor
              (consult-jj-read-commit
               (funcall consult-jj-log-function root)
               "New commit anchor: "))))
  (when anchor
    (consult-jj--placement
     #'consult-jj--new-at-placement
     (list anchor description no-edit root)))
  nil)

;;;###autoload
(defun consult-jj-new-after (&optional anchor description no-edit root)
  "Insert a new empty commit after ANCHOR.
When ANCHOR is nil, read it from the structured Jujutsu log under ROOT.
DESCRIPTION is the new commit description.  When NO-EDIT is non-nil, do not
make the new commit the working-copy commit."
  (interactive
   (list nil (when current-prefix-arg
               (read-string "Description: " ""))))
  (consult-jj--new-with-placement
   anchor 'after description no-edit root))

;;;###autoload
(defun consult-jj-new-before (&optional anchor description no-edit root)
  "Insert a new empty commit before ANCHOR.
When ANCHOR is nil, read it from the structured Jujutsu log under ROOT.
DESCRIPTION is the new commit description.  When NO-EDIT is non-nil, do not
make the new commit the working-copy commit."
  (interactive
   (list nil (when current-prefix-arg
               (read-string "Description: " ""))))
  (consult-jj--new-with-placement
   anchor 'before description no-edit root))

;;;###autoload
(defun consult-jj-new-onto (&optional anchor description no-edit root)
  "Create a new empty child commit of ANCHOR.
When ANCHOR is nil, read it from the structured Jujutsu log under ROOT.
DESCRIPTION is the new commit description.  When NO-EDIT is non-nil, do not
make the new commit the working-copy commit."
  (interactive
   (list nil (when current-prefix-arg
               (read-string "Description: " ""))))
  (consult-jj--new-with-placement
   anchor 'onto description no-edit root))

;;;###autoload
(defun consult-jj-rebase (&optional source destination root selection)
  "Rebase SOURCE relative to DESTINATION using SELECTION and chosen placement.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
When either is nil, read it from the structured Jujutsu log under ROOT.  Then
display the placement transient.  SELECTION is `source' for the commit and its
descendants or `revision' for the commit only."
  (interactive
   (list nil nil nil
         (and (equal current-prefix-arg '(4)) 'source)))
  (consult-jj--validate-rebase-selection selection)
  (when-let ((targets
              (consult-jj--read-rebase-targets
               source destination root "Rebase destination: ")))
    (apply (if selection
               #'consult-jj--open-rebase-placement
             #'consult-jj--rebase-selection)
           (append targets (list selection))))
  nil)

;;;###autoload
(defun consult-jj-rebase-onto (&optional source destination root selection)
  "Rebase SOURCE onto DESTINATION under ROOT using SELECTION.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
Read either omitted value from the structured Jujutsu log.  SELECTION is
  `source' for the commit and its descendants or `revision' for the commit only."
  (interactive
   (or (consult-jj--transient-scope)
       (list nil nil nil
             (and (equal current-prefix-arg '(4)) 'source))))
  (consult-jj--rebase-with-placement
   source destination 'onto root selection))

;;;###autoload
(defun consult-jj-rebase-after (&optional source destination root selection)
  "Rebase SOURCE after DESTINATION under ROOT using SELECTION.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
Read either omitted value from the structured Jujutsu log.  SELECTION is
  `source' for the commit and its descendants or `revision' for the commit only."
  (interactive
   (or (consult-jj--transient-scope)
       (list nil nil nil
             (and (equal current-prefix-arg '(4)) 'source))))
  (consult-jj--rebase-with-placement
   source destination 'after root selection))

;;;###autoload
(defun consult-jj-rebase-before (&optional source destination root selection)
  "Rebase SOURCE before DESTINATION under ROOT using SELECTION.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
Read either omitted value from the structured Jujutsu log.  SELECTION is
  `source' for the commit and its descendants or `revision' for the commit only."
  (interactive
   (or (consult-jj--transient-scope)
       (list nil nil nil
             (and (equal current-prefix-arg '(4)) 'source))))
  (consult-jj--rebase-with-placement
   source destination 'before root selection))

(transient-define-prefix consult-jj--rebase-selection
  (source destination root placement)
  "Choose which commits to rebase."
  ["Selection"
   ("s" "Commit and descendants" consult-jj--rebase-select-source)
   ("r" "Commit only" consult-jj--rebase-select-revision)]
  (interactive (list nil nil nil nil))
  (transient-setup 'consult-jj--rebase-selection nil nil
                   :scope (list source destination root placement)))

(defun consult-jj--rebase-select-source
    (source destination root placement)
  "Resume a rebase of SOURCE and its descendants using saved state."
  (interactive (consult-jj--transient-scope))
  (consult-jj--continue-rebase
   source destination root placement 'source))

(defun consult-jj--rebase-select-revision
    (source destination root placement)
  "Resume a rebase of only SOURCE using saved state."
  (interactive (consult-jj--transient-scope))
  (consult-jj--continue-rebase
   source destination root placement 'revision))

(transient-define-prefix consult-jj--placement (operation arguments)
  "Choose a placement and invoke OPERATION with ARGUMENTS."
  ["Placement"
   ("a" "After" consult-jj--place-after)
   ("b" "Before" consult-jj--place-before)
   ("o" "Onto" consult-jj--place-onto)]
  (interactive (list nil nil))
  (transient-setup 'consult-jj--placement nil nil
                   :scope (list operation arguments)))

(defun consult-jj--place-after (operation arguments)
  "Invoke OPERATION with `after' placement and ARGUMENTS."
  (interactive (consult-jj--transient-scope))
  (apply operation 'after arguments))

(defun consult-jj--place-before (operation arguments)
  "Invoke OPERATION with `before' placement and ARGUMENTS."
  (interactive (consult-jj--transient-scope))
  (apply operation 'before arguments))

(defun consult-jj--place-onto (operation arguments)
  "Invoke OPERATION with `onto' placement and ARGUMENTS."
  (interactive (consult-jj--transient-scope))
  (apply operation 'onto arguments))

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
                        (consult-jj--live-candidate-collection
                         absolute root 'modified-file)
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
                   (consult-jj--live-candidate-collection
                    candidates root 'modified-hunk)
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
  (let ((kind
         (cond
          ((null targets)
           (user-error "consult-jj: restore requires at least one target"))
          ((cl-every #'consult-jj-hunk-p targets) 'hunk)
          ((cl-every #'stringp targets) 'file)
          (t
           (user-error
            "consult-jj: restore targets must all have the same kind")))))
    (setq root
          (or root
              (if (eq kind 'hunk)
                  (consult-jj-hunk-root (car targets))
                (locate-dominating-file (car targets) ".jj"))))
    (unless root
      (user-error "consult-jj: No Jujutsu repository found for restore targets"))
    (setq root (file-name-as-directory (expand-file-name root)))
    (let ((default-directory root)
          (consult-jj--commit-modified-root root))
      (if (eq kind 'hunk)
          (consult-jj-jj--restore-hunks targets root)
        (consult-jj-jj--restore-files targets root))
      (run-hooks 'consult-jj-commit-modified-hook)))
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
    (setq root (file-name-as-directory (expand-file-name root)))
    (setq destination (or destination
                          (consult-jj--read-squash-destination root)))
    (when destination
      (let ((consult-jj--commit-modified-root root))
        (consult-jj--complete-squash
         destination
         (lambda ()
           (if (eq kind 'hunk)
               (consult-jj-jj--squash-hunks targets destination root)
             (consult-jj-jj--squash-files targets destination root)))
         (lambda ()
           (if (eq kind 'hunk)
             (consult-jj-jj--squash-hunks targets destination root t)
             (consult-jj-jj--squash-files targets destination root t))))))
  nil))

;;;###autoload
(defun consult-jj-commit-squash
    (&optional source destination description-policy root)
  "Squash the whole change from SOURCE into DESTINATION.
SOURCE and DESTINATION may be structured commit candidates or revision strings.
DESCRIPTION-POLICY may be `combine', `destination', or the exact description
string to use.  When it is nil, resolve unambiguous descriptions automatically
and ask about two non-empty descriptions.  ROOT is the repository root."
  (interactive)
  (unless (memq consult-jj-squash-immutable-policy '(ask ignore refuse))
    (user-error "consult-jj: Invalid immutable policy `%s'"
                consult-jj-squash-immutable-policy))
  (setq root (or root (consult-jj--root)))
  (let* ((default-directory root)
         (commits
          (and (or (null source) (null destination))
               (funcall consult-jj-log-function root))))
    (setq source
          (or source
              (consult-jj-read-commit commits "Commit to squash: ")))
    (when source
      (let* ((parents
              (and (null destination)
                   (consult-jj-jj--commit-parents
                    (consult-jj--commit-id source) root)))
             (default
              (and (= (length parents) 1) (car parents)))
             (destination-commits
              (if (and
                   default
                   (not
                    (cl-find
                     (consult-jj-commit-commit-id default)
                     commits
                     :key #'consult-jj-commit-commit-id
                     :test #'equal)))
                  (append commits (list default))
                commits)))
        (setq destination
              (or destination
                  (consult-jj-read-commit
                   destination-commits "Squash destination: " default))))
      (when destination
        (setq description-policy
              (consult-jj--resolve-squash-description
               source destination description-policy root))
        (let ((consult-jj--commit-modified-root
               (file-name-as-directory (expand-file-name root))))
          (consult-jj--complete-squash
           (consult-jj--commit-id destination)
           (lambda ()
             (consult-jj-jj--commit-squash
              (consult-jj--commit-id source)
              (consult-jj--commit-id destination)
              description-policy root))
           (lambda ()
             (consult-jj-jj--commit-squash
              (consult-jj--commit-id source)
              (consult-jj--commit-id destination)
              description-policy root t)))))))
  nil)

(defun consult-jj--complete-squash (destination operation ignore-operation)
  "Complete a squash into DESTINATION using the supplied operations.
OPERATION performs the ordinary mutation.  IGNORE-OPERATION performs it while
allowing immutable rewrites."
  (let ((result
         (funcall
          (if (eq consult-jj-squash-immutable-policy 'ignore)
              ignore-operation
            operation))))
    (when (and (eq consult-jj-squash-immutable-policy 'ask)
               (eq result 'immutable)
               (y-or-n-p "Commit is immutable, ignore: "))
      (setq result (funcall ignore-operation)))
    (unless (eq result 'immutable)
      (if (and (integerp result) (> result 0))
          (message "squashed changes into %s with %d conflicts"
                   destination result)
        (message "squashed changes into %s" destination))
      (run-hooks 'consult-jj-commit-modified-hook))))

(defun consult-jj--resolve-squash-description
    (source destination policy root)
  "Resolve POLICY for squashing SOURCE into DESTINATION under ROOT."
  (pcase policy
    ((pred stringp) policy)
    ('destination 'destination)
    ((or 'combine 'nil)
     (let* ((source-description
             (consult-jj--commit-description source root))
            (destination-description
             (consult-jj--commit-description destination root))
            (combined
             (string-join
              (cl-remove-if #'string-empty-p
                            (list destination-description source-description))
              "\n\n")))
       (if (eq policy 'combine)
           combined
         (cond
          ((string-empty-p source-description) 'destination)
          ((string-empty-p destination-description) source-description)
          (t
           (pcase
               (completing-read
                "Squash description: "
                '("Combine descriptions"
                  "Use destination description"
                  "Edit combined description")
                nil t nil nil "Combine descriptions")
             ("Combine descriptions" combined)
             ("Use destination description" 'destination)
             ("Edit combined description"
              (read-string "Combined description: " combined))))))))
    (_
     (user-error "consult-jj: Invalid squash description policy `%s'"
                 policy))))

(defun consult-jj--commit-description (commit root)
  "Return COMMIT's complete description under ROOT."
  (or
   (and (consult-jj-commit-p commit)
        (or (consult-jj-commit-description commit) ""))
   (consult-jj-jj--commit-description (consult-jj--commit-id commit) root)
   ""))

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
  (let* ((root (file-name-as-directory
                (expand-file-name (consult-jj--root))))
         (description (or description (read-string "Description: " "")))
         (final-description
          (or (and consult-jj-description-function
                   (funcall consult-jj-description-function description))
              description))
         (consult-jj--commit-modified-root root))
    (cond
     ((cl-every #'consult-jj-hunk-p targets)
      (consult-jj-jj--split-hunks targets final-description root))
     ((cl-every #'stringp targets)
      (consult-jj-jj--split-files targets final-description root))
     (t
      (user-error "consult-jj: Split targets must all have the same kind")))
    (run-hooks 'consult-jj-commit-modified-hook))
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

(defun consult-jj--new-at-placement
    (placement anchor description no-edit root)
  "Create a new commit relative to ANCHOR using PLACEMENT."
  (pcase placement
    ('after
     (consult-jj-new-after anchor description no-edit root))
    ('before
     (consult-jj-new-before anchor description no-edit root))
    ('onto
     (consult-jj-new-onto anchor description no-edit root))
    (_
     (error "consult-jj: invalid new placement `%s'" placement))))

(defun consult-jj--new-with-placement
    (anchor placement description no-edit root)
  "Create a new commit relative to ANCHOR using PLACEMENT under ROOT."
  (setq root (or root (consult-jj--root)))
  (let ((default-directory root))
    (setq anchor
          (or anchor
              (consult-jj-read-commit
               (funcall consult-jj-log-function root)
               (format "New commit %s: " placement)))))
  (when anchor
    (setq root (file-name-as-directory (expand-file-name root)))
    (let ((consult-jj--commit-modified-root root)
          (final-description
           (and description
                (or (and consult-jj-description-function
                         (funcall consult-jj-description-function description))
                    description))))
      (consult-jj-jj--new
       (consult-jj--commit-id anchor) root placement final-description no-edit)
      (run-hooks 'consult-jj-commit-modified-hook)))
  nil)

(defun consult-jj--duplicate-at-placement
    (placement source destination root)
  "Duplicate SOURCE relative to DESTINATION using PLACEMENT under ROOT."
  (pcase placement
    ('onto
     (consult-jj-commit-duplicate-onto source destination root))
    ('after
     (consult-jj-commit-duplicate-after source destination root))
    ('before
     (consult-jj-commit-duplicate-before source destination root))
    (_
     (error "consult-jj: invalid duplicate placement `%s'" placement))))

(defun consult-jj--duplicate-with-placement
    (source destination placement root)
  "Duplicate SOURCE relative to DESTINATION using PLACEMENT under ROOT."
  (when-let ((targets
              (consult-jj--read-duplicate-targets
               source destination root
               (format "Duplicate %s: " placement))))
    (pcase-let ((`(,source ,destination ,root) targets))
      (consult-jj--perform-duplicate
       source destination placement root)))
  nil)

(defun consult-jj--read-duplicate-targets
    (source destination root destination-prompt)
  "Return SOURCE, DESTINATION, and ROOT, reading omitted duplicate targets.
DESTINATION-PROMPT labels the structured-log destination selection."
  (setq root (or root (consult-jj--root)))
  (let* ((default-directory root)
         (commits
          (and (or (null source) (null destination))
               (funcall consult-jj-log-function root))))
    (setq source
          (or source
              (consult-jj-read-commit commits "Commit to duplicate: ")))
    (when source
      (setq destination
            (or destination
                (consult-jj-read-commit commits destination-prompt)))
      (when destination
        (list source destination root)))))

(defun consult-jj--perform-duplicate
    (source destination placement root)
  "Duplicate SOURCE relative to DESTINATION using PLACEMENT under ROOT.
Run `consult-jj-commit-modified-hook' after Jujutsu succeeds."
  (setq root (file-name-as-directory (expand-file-name root)))
  (let ((consult-jj--commit-modified-root root))
    (consult-jj-jj--duplicate
     (consult-jj--commit-id source) root
     (and destination (consult-jj--commit-id destination))
     placement)
    (run-hooks 'consult-jj-commit-modified-hook)))

(defun consult-jj--continue-rebase
    (source destination root placement selection)
  "Resume a rebase with saved state and an explicit SELECTION."
  (if placement
      (consult-jj--rebase-at-placement
       placement source destination root selection)
    (consult-jj--open-rebase-placement
     source destination root selection)))

(defun consult-jj--open-rebase-placement
    (source destination root selection)
  "Choose placement for rebasing SOURCE relative to DESTINATION."
  (consult-jj--placement
   #'consult-jj--rebase-at-placement
   (list source destination root selection)))

(defun consult-jj--rebase-at-placement
    (placement source destination root selection)
  "Rebase SOURCE relative to DESTINATION using PLACEMENT."
  (pcase placement
    ('onto
     (consult-jj-rebase-onto
      source destination root selection))
    ('after
     (consult-jj-rebase-after
      source destination root selection))
    ('before
     (consult-jj-rebase-before
      source destination root selection))
    (_
     (error "consult-jj: invalid rebase placement `%s'" placement))))

(defun consult-jj--rebase-with-placement
    (source destination placement root selection)
  "Rebase SOURCE at DESTINATION using PLACEMENT and SELECTION under ROOT."
  (consult-jj--validate-rebase-selection selection)
  (when-let ((targets
              (consult-jj--read-rebase-targets
               source destination root (format "Rebase %s: " placement))))
    (pcase-let ((`(,source ,destination ,root) targets))
      (if selection
          (let ((consult-jj--commit-modified-root
                 (file-name-as-directory (expand-file-name root))))
            (consult-jj-jj--rebase
             (consult-jj--commit-id source)
             (consult-jj--commit-id destination)
             placement root selection)
            (run-hooks 'consult-jj-commit-modified-hook))
        (consult-jj--rebase-selection
         source destination root placement))))
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

(defun consult-jj--validate-rebase-selection (selection)
  "Signal an error unless SELECTION is nil, `source', or `revision'."
  (unless (memq selection '(nil source revision))
    (error "consult-jj: invalid rebase selection `%s'" selection)))

(defun consult-jj--transient-scope ()
  "Return the active Transient scope, or nil outside a transient."
  (when (bound-and-true-p transient-current-prefix)
    (transient-scope)))

(defun consult-jj--root ()
  "Return the current project root, or signal a `user-error'."
  (let ((project (project-current nil)))
    (unless project
      (user-error "consult-jj: No project found for %s" default-directory))
    (expand-file-name (project-root project))))

(defun consult-jj--refresh-live-candidate-sessions ()
  "Refresh live sessions for `consult-jj--commit-modified-root'."
  (when consult-jj--commit-modified-root
    (setq consult-jj--candidate-sessions
          (cl-delete-if-not
           (lambda (session)
             (buffer-live-p
              (consult-jj--candidate-session-buffer session)))
           consult-jj--candidate-sessions))
    (let ((sessions
           (cl-remove-if-not
            (lambda (session)
              (and
               (memq (consult-jj--candidate-session-view session)
                     '(modified-file modified-hunk log))
               (equal (consult-jj--candidate-session-root session)
                      consult-jj--commit-modified-root)))
            consult-jj--candidate-sessions)))
      (when sessions
        (let* ((root consult-jj--commit-modified-root)
               (modified-p
                (cl-find-if
                 (lambda (session)
                   (memq (consult-jj--candidate-session-view session)
                         '(modified-file modified-hunk)))
                 sessions))
               (log-p
                (cl-find-if
                 (lambda (session)
                   (eq (consult-jj--candidate-session-view session) 'log))
                 sessions))
               (hunks (and modified-p (consult-jj-collect-hunks root)))
               (commits (and log-p (funcall consult-jj-log-function root)))
               (log-candidates (consult-jj--commit-candidates commits)))
          (dolist (session sessions)
            (with-current-buffer
                (consult-jj--candidate-session-buffer session)
              (funcall
               (consult-jj--candidate-session-replace session)
               (pcase (consult-jj--candidate-session-view session)
                 ('modified-file
                  (mapcar #'car
                          (consult-jj--group-hunks-by-file hunks root)))
                 ('modified-hunk
                  (mapcar
                   (lambda (hunk)
                     (consult-jj--hunk-candidate hunk root))
                   hunks))
                 ('log log-candidates)))
              (run-hooks
               'consult-jj-candidate-session-refreshed-hook))))))))

(defun consult-jj--live-candidate-collection (initial root view)
  "Return a live Consult collection for INITIAL candidates under ROOT.
VIEW identifies the candidate presentation to refresh."
  (lambda (sink)
    (let ((candidates initial)
          (input "")
          session)
      (lambda (action)
        (pcase action
          ('setup
           (funcall sink action)
           (setq session
                 (consult-jj--candidate-session-create
                  :root (file-name-as-directory (expand-file-name root))
                  :view view
                  :buffer (current-buffer)
                  :replace
                  (lambda (replacement)
                    (setq candidates replacement)
                    (consult-jj--replace-live-candidates
                     sink candidates input))))
           (push session consult-jj--candidate-sessions)
           nil)
          ('destroy
           (setq consult-jj--candidate-sessions
                 (delq session consult-jj--candidate-sessions))
           (funcall sink action))
          ((pred stringp)
           (setq input action)
           (consult-jj--replace-live-candidates sink candidates input))
          (_ (funcall sink action)))))))

(defun consult-jj--replace-live-candidates (sink candidates input)
  "Replace SINK contents with CANDIDATES matching INPUT."
  (funcall sink 'flush)
  (when-let ((matching
              (consult-jj--filter-live-candidates candidates input)))
    (funcall sink matching))
  (funcall sink 'refresh))

(defun consult-jj--filter-live-candidates (candidates input)
  "Return CANDIDATES matching Consult INPUT."
  (pcase-let ((`(,regexps . ,highlight)
               (consult--compile-regexp
                input 'emacs completion-ignore-case)))
    (if regexps
        (let* ((completion-regexp-list regexps)
               (matching (all-completions "" candidates)))
          (cl-loop for candidate in-ref matching
                   do (funcall highlight
                               (setf candidate (copy-sequence candidate))))
          matching)
      (copy-sequence candidates))))

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

(defun consult-jj--commit-candidates (commits)
  "Build completion candidates for structured COMMITS in source order."
  (cl-loop for commit in commits
           for index from 0
           collect (consult-jj--commit-candidate commit index)))

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

(add-hook 'consult-jj-commit-modified-hook
          #'consult-jj--refresh-live-candidate-sessions)

(provide 'consult-jj)
;;; consult-jj.el ends here
