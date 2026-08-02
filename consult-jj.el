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
(require 'diff-mode)
(require 'consult)
(require 'transient)

(defgroup consult-jj nil
  "Browse Jujutsu changes with Consult."
  :group 'tools
  :prefix "consult-jj-")

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

(defcustom consult-jj-view-file-buffer-name "*consult-jj-revision-file*"
  "Name of the reusable buffer used for revision file views."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-squash-immutable-policy 'ask
  "How `consult-jj-squash' handles an immutable commit.
The value `ask' requests confirmation, `ignore' allows the rewrite without
confirmation, and `refuse' cancels the squash without confirmation."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Ignore" ignore)
                 (const :tag "Refuse" refuse))
  :group 'consult-jj)

(defcustom consult-jj-bookmark-function #'consult-jj-collect-bookmarks
  "Function used by `consult-jj-bookmark' to collect bookmark candidates.
The function receives the repository root and must return a list of
`consult-jj-bookmark' objects."
  :type 'function
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

(defconst consult-jj--diff-preview-buffer-name "*consult-jj-diff-preview*"
  "Base name of the temporary modified-change preview buffer.")

(defvar consult-jj--commit-modified-root nil
  "Repository root for the current commit-modification notification.")

(require 'consult-jj-core)
(require 'consult-jj-bookmark)
(require 'consult-jj-commit)
(require 'consult-jj-hunk)
(require 'consult-jj-modified-file)
(require 'consult-jj-jj)
(require 'consult-jj-git)
(require 'consult-jj-session)
(require 'consult-jj-log)
(require 'consult-jj-tag)
(require 'consult-jj-op-log)
(require 'consult-jj-workspace)

;;;###autoload
(defun consult-jj-bookmark ()
  "Select a Jujutsu bookmark and create a new child commit there."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (bookmarks (funcall consult-jj-bookmark-function root)))
    (if (null bookmarks)
        (message "No Jujutsu bookmarks found.")
      (when-let* ((selected (consult-jj--read-bookmark bookmarks nil root)))
        (consult-jj-new-here
         (consult-jj-bookmark-revision selected) nil nil root))))
  nil)

;;;###autoload
(defun consult-jj-bookmark-move (&optional source destination)
  "Move local bookmark SOURCE to DESTINATION.
SOURCE is a structured bookmark and DESTINATION is a structured commit.
Lisp callers that supply both arguments do not prompt."
  (interactive)
  (let* ((root (file-name-as-directory (expand-file-name (consult-jj--root))))
         (default-directory root)
         (bookmarks
          (unless source
            (funcall consult-jj-bookmark-function root)))
         (source
          (or source
              (consult-jj-read-bookmark
               bookmarks "Bookmark to move: "))))
    (when source
      (setq destination
            (or destination
                (consult-jj-read-commit
                 (funcall consult-jj-log-function root 'default)
                 "Move bookmark to: ")))
      (when destination
        (let ((track-p
               (and
                (consult-jj-bookmark-remote source)
                (not
                 (consult-jj--local-bookmark-named-p
                  (consult-jj-bookmark-name source)
                  (or bookmarks
                      (funcall consult-jj-bookmark-function root)))))))
          (when (or
                 (not track-p)
                 (y-or-n-p
                  (format "Track remote bookmark `%s'? "
                          (consult-jj-bookmark-revision source))))
            (consult-jj--move-bookmark
             source destination root track-p))))))
  nil)

;;;###autoload
(defun consult-jj-bookmark-set (&optional target name)
  "Set local bookmark NAME at TARGET.
TARGET is a structured bookmark, structured commit, or Jujutsu revision
string.  With a universal prefix, use the working-copy commit without prompting
for TARGET.  With a positive numeric prefix N, use the Nth first-parent
ancestor, written as `@' followed by N minus signs.  Lisp callers that supply
TARGET and NAME do not prompt."
  (interactive
   (list
    (cond
     ((consp current-prefix-arg) "@")
     ((integerp current-prefix-arg)
      (concat "@" (make-string current-prefix-arg ?-))))))
  (let* ((root (file-name-as-directory (expand-file-name (consult-jj--root))))
         (default-directory root)
         (target (or target (consult-jj--read-bookmark-set-target root))))
    (when target
      (setq name (or name (consult-jj--read-bookmark-set-name target root)))
      (let ((consult-jj--commit-modified-root root))
        (consult-jj-jj--bookmark-set
         name (consult-jj--bookmark-set-revision target) root)
        (run-hooks 'consult-jj-commit-modified-hook))))
  nil)

;;;###autoload
(defun consult-jj-bookmark-advance (&optional targets)
  "Advance eligible local bookmarks using Jujutsu configuration.
TARGETS may be a homogeneous list of local bookmark objects or local
bookmark-name strings.  When nil, leave both eligibility and destination to
Jujutsu configuration."
  (interactive)
  (let ((root (consult-jj--root))
        (names (consult-jj--bookmark-advance-names targets)))
    (when (consult-jj-jj--bookmark-advance names root)
      (let ((consult-jj--commit-modified-root root))
        (run-hooks 'consult-jj-commit-modified-hook))))
  nil)

(defun consult-jj-read-bookmark (bookmarks &optional prompt default)
  "Read and return one structured bookmark from BOOKMARKS, or nil.
PROMPT defaults to `Jujutsu bookmarks: '.  DEFAULT, when non-nil, is the
bookmark offered as the default candidate."
  (consult-jj--read-bookmark bookmarks prompt nil default))

(defun consult-jj--read-bookmark
    (bookmarks prompt &optional live-root default)
  "Read one of BOOKMARKS using PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable bookmark session there.
DEFAULT, when non-nil, is the bookmark offered as the default candidate."
  (let* ((candidates (consult-jj--bookmark-candidates bookmarks))
         (default-candidate
          (and default
               (cl-find-if
                (lambda (candidate)
                  (equal
                   (consult-jj-bookmark-revision
                    (get-text-property 0 'consult-jj-bookmark candidate))
                   (consult-jj-bookmark-revision default)))
                candidates))))
    (when candidates
      (consult--read
       (if live-root
           (consult-jj--live-candidate-collection
            candidates live-root 'bookmark nil
            #'consult-jj--collect-session-bookmarks
            #'consult-jj--present-session-bookmarks)
         candidates)
       :prompt (or prompt "Jujutsu bookmarks: ")
       :category 'consult-jj-bookmark
       :require-match t
       :sort nil
       :lookup #'consult-jj--lookup-bookmark
       :default default-candidate
       :history '(:input consult--line-history)))))

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
               (funcall consult-jj-log-function root 'default)
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
               (funcall consult-jj-log-function root 'default)
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
               (funcall consult-jj-log-function root 'default)
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
               (funcall consult-jj-log-function root 'default)
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
  (when-let* ((targets
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
(defun consult-jj-modified-files-in-commit (&optional commit)
  "Pick a modified file introduced by structured source COMMIT.
When COMMIT is nil, read one through the shared structured commit reader."
  (interactive)
  (let ((root (consult-jj--root)))
    (let ((default-directory root))
      (setq commit
            (or commit
                (consult-jj-read-commit
                 (funcall consult-jj-log-function root 'default)
                 "Source commit: "))))
    (when commit
      (consult-jj--browse-modified-files
       root (consult-jj--commit-source-selector commit)))))

;;;###autoload
(defun consult-jj-modified-files ()
  "Pick a modified file in the current project with Consult preview.
Files come from the Jujutsu working-copy commit `@'.  With a universal prefix,
prompt for a source commit and browse the files introduced by that commit."
  (interactive)
  (if (equal current-prefix-arg '(4))
      (consult-jj-modified-files-in-commit)
    (consult-jj--browse-modified-files (consult-jj--root) "@")))

(defun consult-jj--browse-modified-files (root source-rev)
  "Browse files modified by SOURCE-REV under ROOT."
  (let* ((root (file-name-as-directory root))
         (default-directory root)
         (files
          (if (equal source-rev "@")
              (consult-jj-collect-modified-files root)
            (consult-jj-collect-modified-files root source-rev))))
    (if (null files)
        (message "No modified files found.")
      (let* ((groups
              (mapcar
               (lambda (file)
                 (cons
                  (expand-file-name
                   (or (consult-jj-modified-file-after-path file)
                       (consult-jj-modified-file-before-path file))
                   root)
                  (consult-jj-modified-file-hunks file)))
               files))
             (candidates
              (mapcar
               (lambda (file)
                 (consult-jj--modified-file-candidate file root))
               files))
             (selected (consult--read
                        (consult-jj--live-candidate-collection
                         candidates root 'modified-file nil
                         #'consult-jj--collect-session-modified-files
                         #'consult-jj--present-session-files
                         source-rev)
                        :prompt "Modified files: "
                        :category 'consult-jj-modified-file
                        :require-match t
                        :sort nil
                        :lookup #'consult-jj--lookup-modified-file
                        :state
                        (consult-jj--modified-file-preview-state
                         groups root source-rev)
                        :history 'file-name-history)))
        (when selected
          (let ((path
                 (or (consult-jj-modified-file-after-path selected)
                     (consult-jj-modified-file-before-path selected))))
            (if (equal source-rev "@")
                (find-file (expand-file-name path root))
              (consult-jj-view-file-in-revision source-rev path))))))))

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
      (when-let* ((selected
                  (consult--read
                   (consult-jj--live-candidate-collection
                    candidates root 'modified-hunk nil
                    #'consult-jj--collect-session-modified-files
                    #'consult-jj--present-session-hunks
                    "@")
                   :prompt "Modified hunks: "
                   :category 'consult-jj-modified-hunk
                   :require-match t
                   :sort nil
                   :lookup #'consult-jj--lookup-hunk
                   :history '(:input consult--line-history)
                   :state (consult-jj--modified-hunk-preview-state candidates))))
        (consult-jj-visit-hunk selected root)))))

;;;###autoload
(defun consult-jj-view-diff-in-revision (&optional revision file)
  "Display FILE's introduced change at REVISION in a diff buffer.
Interactively, read either missing value from the current project."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root))
    (setq revision
          (consult-jj--commit-source-selector
           (or revision (consult-jj--read-revision root))))
    (when revision
      (setq file
            (or file
                (consult-jj--read-modified-file-at-revision revision root)))
      (when file
        (let ((diff
               (consult-jj-jj--diff-files
                (list file) root revision)))
          (if (string-empty-p diff)
              (progn
                (message "No change found for `%s' at revision `%s'."
                         file revision)
                nil)
            (consult-jj--display-diff
             diff consult-jj-hunk-diff-buffer-name root)))))))

;;;###autoload
(defun consult-jj-view-file-in-revision (&optional revision file)
  "Display FILE's complete contents at REVISION in a read-only buffer.
Interactively, read either missing value from the current project."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root))
    (setq revision
          (consult-jj--commit-source-selector
           (or revision (consult-jj--read-revision root))))
    (when revision
      (setq file
            (or file (consult-jj--read-file-at-revision revision root)))
      (when file
        (consult-jj--display-revision-file
         (consult-jj-jj--run
          root "--ignore-working-copy"
          "file" "show" "--revision" revision "--" file)
         root)))))

;;;###autoload
(cl-defun consult-jj-diff (targets &key source-rev root)
  "Display a persistent Git-format diff for modified TARGETS under ROOT.
TARGETS must be a homogeneous target set of `consult-jj-modified-file' or
`consult-jj-hunk' objects.  Hunk targets are rendered from their captured diff
snapshot; file targets are read from SOURCE-REV, which defaults to the
working-copy commit `@'.  When ROOT is nil, resolve it through
`consult-jj--root'."
  (interactive
   (let ((root (consult-jj--root)))
     (list (consult-jj-collect-hunks root) :source-rev "@" :root root)))
  (when (null targets)
    (user-error "consult-jj: Diff requires at least one target"))
  (setq root
        (file-name-as-directory
         (expand-file-name (or root (consult-jj--root)))))
  (setq source-rev (or source-rev "@"))
  (let* ((kind (consult-jj--modified-target-kind targets "Diff"))
         (diff
          (if (eq kind 'hunk)
              (consult-jj-hunk->diff targets)
            (consult-jj-jj--diff-files
             (mapcar #'consult-jj--modified-file-path targets)
             root source-rev))))
    (consult-jj--display-diff diff consult-jj-hunk-diff-buffer-name root)))

;;;###autoload
(cl-defun consult-jj-restore (targets &key source-rev root)
  "Restore modified-file or modified-hunk TARGETS in Jujutsu.
TARGETS must be a homogeneous target set of structured modified files or
hunks.  SOURCE-REV defaults to `@'; historical sources are reserved for the
source-aware Restore issue.  ROOT defaults through `consult-jj--root'.
Interactively, restore all modified hunks in the current project."
  (interactive
   (let ((root (consult-jj--root)))
     (list (consult-jj-collect-hunks root) :source-rev "@" :root root)))
  (when (null targets)
    (user-error "consult-jj: Restore requires at least one target"))
  (let ((kind (consult-jj--modified-target-kind targets "Restore")))
    (consult-jj--require-working-copy-source source-rev "Restore")
    (setq root
          (file-name-as-directory
           (expand-file-name (or root (consult-jj--root)))))
    (let ((default-directory root)
          (consult-jj--commit-modified-root root))
      (if (eq kind 'hunk)
          (consult-jj-jj--restore-hunks targets root)
        (consult-jj-jj--restore-files
         (mapcar #'consult-jj--modified-file-path targets) root))
      (run-hooks 'consult-jj-commit-modified-hook)))
  nil)

;;;###autoload
(cl-defun consult-jj-squash
    (targets &key destination source-rev root)
  "Squash modified-file or modified-hunk TARGETS into DESTINATION in Jujutsu.
DESTINATION is a Jujutsu revset.  When it is nil, read a destination from the
repository log.  SOURCE-REV defaults to `@'; historical sources are reserved
for the source-aware Squash issue.  ROOT defaults through `consult-jj--root'.
`consult-jj-squash-immutable-policy' controls immutable rewrites."
  (interactive
   (let ((root (consult-jj--root)))
     (list (consult-jj-collect-hunks root)
           :destination nil :source-rev "@" :root root)))
  (when (null targets)
    (user-error "consult-jj: Squash requires at least one target"))
  (unless (memq consult-jj-squash-immutable-policy '(ask ignore refuse))
    (user-error "consult-jj: Invalid immutable policy `%s'"
                consult-jj-squash-immutable-policy))
  (let ((kind (consult-jj--modified-target-kind targets "Squash")))
    (consult-jj--require-working-copy-source source-rev "Squash")
    (setq root
          (file-name-as-directory
           (expand-file-name (or root (consult-jj--root)))))
    (setq destination (or destination
                          (consult-jj--read-squash-destination root)))
    (when destination
      (let ((consult-jj--commit-modified-root root))
        (consult-jj--complete-squash
         destination
         (lambda ()
           (if (eq kind 'hunk)
               (consult-jj-jj--squash-hunks targets destination root)
             (consult-jj-jj--squash-files
              (mapcar #'consult-jj--modified-file-path targets)
              destination root)))
         (lambda ()
           (if (eq kind 'hunk)
             (consult-jj-jj--squash-hunks targets destination root t)
             (consult-jj-jj--squash-files
              (mapcar #'consult-jj--modified-file-path targets)
              destination root t))))))
  nil))

;;;###autoload
(defun consult-jj-commit-squash
    (&optional source destination description-policy root)
  "Squash whole changes from SOURCE into DESTINATION.
SOURCE may be one structured commit candidate or revision string, or a list of
them.  DESTINATION may be a structured commit candidate or revision string.
DESCRIPTION-POLICY may be `combine', `destination', or the exact description
string to use.  With one source and a nil policy, resolve unambiguous
descriptions automatically and ask about two non-empty descriptions.  With
multiple sources and a nil policy, keep the destination description.  ROOT is
the repository root."
  (interactive)
  (unless (memq consult-jj-squash-immutable-policy '(ask ignore refuse))
    (user-error "consult-jj: Invalid immutable policy `%s'"
                consult-jj-squash-immutable-policy))
  (setq root (or root (consult-jj--root)))
  (let* ((default-directory root)
         (commits
          (and (or (null source) (null destination))
               (funcall consult-jj-log-function root 'default))))
    (setq source
          (or source
              (consult-jj-read-commit commits "Commit to squash: ")))
    (when source
      (setq source (if (listp source) source (list source)))
      (let* ((source-revset
              (if (cdr source)
                  (string-join
                   (mapcar
                    (lambda (commit)
                      (format "(%s)" (consult-jj--commit-id commit)))
                    source)
                   "|")
                (consult-jj--commit-id (car source))))
             (parents
              (and (null destination)
                   (null (cdr source))
                   (consult-jj-jj--commit-parents
                    source-revset root)))
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
                   destination-commits "Squash destination: " default)))
        (when destination
          (when (and (cdr source) (null description-policy))
            (setq description-policy 'destination))
          (setq description-policy
                (consult-jj--resolve-squash-description
                 source destination description-policy root))
          (let ((consult-jj--commit-modified-root
                 (file-name-as-directory (expand-file-name root))))
            (consult-jj--complete-squash
             destination
             (lambda ()
               (consult-jj-jj--commit-squash
                source-revset
                (consult-jj--commit-id destination)
                description-policy root))
             (lambda ()
               (consult-jj-jj--commit-squash
                source-revset
                (consult-jj--commit-id destination)
                description-policy root t))))))))
  nil)

(defun consult-jj--complete-squash (destination operation ignore-operation)
  "Complete a squash into DESTINATION using the supplied operations.
OPERATION performs the ordinary mutation.  IGNORE-OPERATION performs it while
allowing immutable rewrites."
  (let ((consult-jj-jj--squash-short-change-id nil))
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
        (let ((short-change-id
               (or consult-jj-jj--squash-short-change-id
                   (and (consult-jj-commit-p destination)
                        (consult-jj-commit-short-change-id destination))
                   (consult-jj--commit-id destination))))
          (if (and (integerp result) (> result 0))
              (message "squashed changes into %s with %d conflicts"
                       short-change-id result)
            (message "squashed changes into %s" short-change-id)))
        (run-hooks 'consult-jj-commit-modified-hook)))))

(defun consult-jj--resolve-squash-description
    (source destination policy root)
  "Resolve POLICY for squashing SOURCE targets into DESTINATION under ROOT."
  (pcase policy
    ((pred stringp) policy)
    ('destination 'destination)
    ((or 'combine 'nil)
     (let* ((sources (if (listp source) source (list source)))
            (source-descriptions
             (mapcar
              (lambda (commit)
                (consult-jj--commit-description commit root))
              sources))
            (source-description (car source-descriptions))
            (destination-description
             (consult-jj--commit-description destination root))
            (combined
             (string-join
              (cl-remove-if #'string-empty-p
                            (cons destination-description
                                  source-descriptions))
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
(cl-defun consult-jj-split
    (targets &key description source-rev root)
  "Split modified-file or modified-hunk TARGETS into a new child commit.
TARGETS must be a homogeneous target set of structured modified files or
hunks.  The selected changes remain in the original commit and receive
DESCRIPTION; the remaining changes move into a new child commit.  SOURCE-REV
defaults to `@'; historical sources are reserved for the source-aware Split
issue.  ROOT defaults through `consult-jj--root'.  When DESCRIPTION is nil,
read it from the minibuffer."
  (interactive
   (let ((root (consult-jj--root)))
     (list (consult-jj-collect-hunks root)
           :description nil :source-rev "@" :root root)))
  (when (null targets)
    (user-error "consult-jj: Split requires at least one target"))
  (consult-jj--require-working-copy-source source-rev "Split")
  (let* ((kind (consult-jj--modified-target-kind targets "Split"))
         (root (file-name-as-directory
                (expand-file-name (or root (consult-jj--root)))))
         (description (or description (read-string "Description: " "")))
         (final-description
          (or (and consult-jj-description-function
                   (funcall consult-jj-description-function description))
              description))
         (consult-jj--commit-modified-root root))
    (if (eq kind 'hunk)
        (consult-jj-jj--split-hunks targets final-description root)
      (consult-jj-jj--split-files
       (mapcar #'consult-jj--modified-file-path targets)
       final-description root))
    (run-hooks 'consult-jj-commit-modified-hook))
  nil)

(defun consult-jj--read-squash-destination (root)
  "Read a squash destination from the Jujutsu log under ROOT."
  (let* ((default-directory root)
         (commits
          (cl-remove-if #'consult-jj-commit-current-p
                        (funcall consult-jj-log-function root 'default)))
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
               (funcall consult-jj-log-function root 'default)
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
  (when-let* ((targets
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
               (funcall consult-jj-log-function root 'default))))
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
  (when-let* ((targets
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
                       (funcall consult-jj-log-function root 'default))))
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

(defun consult-jj--commit-source-selector (commit)
  "Return the source selector represented by COMMIT.
A divergent structured commit uses its full change ID plus change offset."
  (if (consult-jj-commit-p commit)
      (let ((change-id (or (consult-jj-commit-change-id commit)
                           (consult-jj-commit-commit-id commit))))
        (if (consult-jj-commit-divergent-p commit)
            (format "%s/%d"
                    change-id
                    (consult-jj-commit-change-offset commit))
          change-id))
    commit))

(defun consult-jj--read-bookmark-set-target (root)
  "Read and return a bookmark destination commit under ROOT."
  (let (target)
    (let ((default-directory root)
          (consult-jj-log-visit-function
           (lambda (commit-id)
             (setq target commit-id))))
      (consult-jj-log))
    target))

(defun consult-jj--bookmark-advance-names (targets)
  "Return local bookmark names represented by homogeneous TARGETS."
  (cond
   ((null targets) nil)
   ((cl-every #'stringp targets) targets)
   ((cl-every #'consult-jj-bookmark-p targets)
    (mapcar
     (lambda (bookmark)
       (when (consult-jj-bookmark-remote bookmark)
         (user-error
          "consult-jj: Cannot advance remote bookmark `%s'"
          (consult-jj-bookmark-revision bookmark)))
       (consult-jj-bookmark-name bookmark))
     targets))
   (t
    (user-error
     "consult-jj: Bookmark advancement targets must have one representation"))))

(defun consult-jj--local-bookmark-named-p (name bookmarks)
  "Return non-nil when BOOKMARKS contains a local bookmark named NAME."
  (cl-find-if
   (lambda (bookmark)
     (and (null (consult-jj-bookmark-remote bookmark))
          (equal (consult-jj-bookmark-name bookmark) name)))
   bookmarks))

(defun consult-jj--move-bookmark (source destination root track-p)
  "Move bookmark SOURCE to DESTINATION under ROOT.
When TRACK-P is non-nil, first track SOURCE's remote bookmark.  Notify after
the move, or after tracking when the subsequent move fails."
  (let ((consult-jj--commit-modified-root root))
    (when track-p
      (consult-jj-jj--bookmark-track
       (consult-jj-bookmark-revision source) root))
    (condition-case error-data
        (consult-jj-jj--bookmark-move
         (consult-jj-bookmark-name source)
         (consult-jj--commit-id destination)
         root)
      (error
       (when track-p
         (run-hooks 'consult-jj-commit-modified-hook))
       (signal (car error-data) (cdr error-data))))
    (run-hooks 'consult-jj-commit-modified-hook)))

(defun consult-jj--read-bookmark-set-name (target root)
  "Read a local bookmark name for TARGET under ROOT."
  (completing-read
   "Bookmark name: "
   (mapcar
    #'consult-jj-bookmark-name
    (cl-remove-if
     #'consult-jj-bookmark-remote
     (funcall consult-jj-bookmark-function root)))
   nil nil
   (if (consult-jj-bookmark-p target)
       (or (and (consult-jj-bookmark-remote target)
                (consult-jj-bookmark-name target))
           "")
     "")))

(defun consult-jj--bookmark-set-revision (target)
  "Return the Jujutsu revision represented by bookmark TARGET."
  (cond
   ((consult-jj-bookmark-p target)
    (consult-jj-bookmark-revision target))
   ((consult-jj-commit-p target)
    (consult-jj-commit-commit-id target))
   ((stringp target) target)
   (t
    (user-error "consult-jj: Invalid bookmark destination"))))

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

(defun consult-jj--modified-target-kind (targets operation)
  "Return the homogeneous kind of TARGETS for OPERATION.
The result is either `file' or `hunk'."
  (cond
   ((cl-every #'consult-jj-modified-file-p targets) 'file)
   ((cl-every #'consult-jj-hunk-p targets) 'hunk)
   (t
    (user-error
     "consult-jj: %s targets must be homogeneous structured modified files or hunks"
     operation))))

(defun consult-jj--modified-file-path (file)
  "Return FILE's current operation path."
  (or (consult-jj-modified-file-after-path file)
      (consult-jj-modified-file-before-path file)
      (user-error "consult-jj: Modified-file target has no path")))

(defun consult-jj--require-working-copy-source (source-rev operation)
  "Return normalized SOURCE-REV when OPERATION supports it.
Signal a `user-error' for historical sources whose semantics belong to a later
issue."
  (setq source-rev (or source-rev "@"))
  (unless (equal source-rev "@")
    (user-error
     "consult-jj: %s does not yet support historical source `%s'"
     operation source-rev))
  source-rev)

(defun consult-jj--refresh-live-candidate-sessions ()
  "Refresh live sessions for `consult-jj--commit-modified-root'."
  (when consult-jj--commit-modified-root
    (consult-jj--refresh-candidate-sessions-once
     consult-jj--commit-modified-root)))

(defun consult-jj--collect-session-modified-files (root _tier source-rev)
  "Collect structured modified files from SOURCE-REV under ROOT."
  (consult-jj-collect-modified-files root source-rev))

(defun consult-jj--modified-file-candidate (file root)
  "Present structured FILE relative to ROOT while retaining its model and path."
  (let ((absolute
         (expand-file-name
          (or (consult-jj-modified-file-after-path file)
              (consult-jj-modified-file-before-path file))
          root)))
    (propertize
     (file-relative-name absolute root)
     'consult-jj-file absolute
     'consult-jj-modified-file file)))

(defun consult-jj--lookup-modified-file (selected candidates &rest _)
  "Return the structured modified file for SELECTED in CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-modified-file candidate)))

(defun consult-jj--present-session-files (files root)
  "Present structured FILES under ROOT as modified-file candidates."
  (mapcar
   (lambda (file)
     (consult-jj--modified-file-candidate file root))
   files))

(defun consult-jj--present-session-hunks (files root)
  "Present the ordered hunks retained by structured FILES under ROOT."
  (mapcar
   (lambda (hunk)
     (consult-jj--hunk-candidate hunk root))
   (mapcan
    (lambda (file)
      (copy-sequence (consult-jj-modified-file-hunks file)))
    files)))

(defun consult-jj--collect-session-bookmarks (root _tier _source-rev)
  "Collect structured bookmarks under ROOT for a live session."
  (funcall consult-jj-bookmark-function root))

(defun consult-jj--present-session-bookmarks (bookmarks _root)
  "Present structured BOOKMARKS as completion candidates."
  (consult-jj--bookmark-candidates bookmarks))

(consult-jj--register-candidate-session-adapter
 'modified-file
 #'consult-jj--collect-session-modified-files
 #'consult-jj--present-session-files)

(consult-jj--register-candidate-session-adapter
 'modified-hunk
 #'consult-jj--collect-session-modified-files
 #'consult-jj--present-session-hunks)

(consult-jj--register-candidate-session-adapter
 'bookmark
 #'consult-jj--collect-session-bookmarks
 #'consult-jj--present-session-bookmarks)

(defun consult-jj--bookmark-candidate (bookmark index)
  "Build a completion candidate for BOOKMARK disambiguated by INDEX."
  (let ((candidate
         (consult--tofu-append (consult-jj-bookmark-revision bookmark) index)))
    (add-text-properties 0 1 (list 'consult-jj-bookmark bookmark) candidate)
    candidate))

(defun consult-jj--bookmark-candidates (bookmarks)
  "Build completion candidates for structured BOOKMARKS in source order."
  (cl-loop for bookmark in bookmarks
           for index from 0
           collect (consult-jj--bookmark-candidate bookmark index)))

(defun consult-jj--lookup-bookmark (selected candidates &rest _)
  "Return the bookmark object for SELECTED from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-bookmark candidate)))

(defun consult-jj--modified-file-preview-state (groups root source-rev)
  "Return the configured modified-file preview state for GROUPS.
GROUPS is an alist of absolute file names to captured hunks.  ROOT and
SOURCE-REV identify the source used by content previews."
  (pcase consult-jj-modified-files-preview-style
    ('diff
     (consult-jj--diff-preview-state
      (lambda (file)
        (consult-jj-hunk->diff
         (if (consult-jj-modified-file-p file)
             (consult-jj-modified-file-hunks file)
           (cdr
            (assoc-string
             (or (get-text-property 0 'consult-jj-file file) file)
             groups)))))
      consult-jj--diff-preview-buffer-name))
    ('visit
     (if (equal source-rev "@")
         (let ((preview (consult--file-preview)))
           (lambda (action file)
             (funcall
              preview action
              (if (consult-jj-modified-file-p file)
                  (or (consult-jj-modified-file-after-path file)
                      (consult-jj-modified-file-before-path file))
                file))))
       (consult-jj--revision-file-preview-state root source-rev)))
    ('none nil)
    (style
     (user-error "consult-jj: Invalid modified-file preview style `%s'" style))))

(defun consult-jj--revision-file-preview-state (root source-rev)
  "Return a transient file-content preview state for SOURCE-REV under ROOT."
  (let ((preview (consult--buffer-preview))
        buffer)
    (lambda (action file)
      (when (and (eq action 'preview) file)
        (unless (buffer-live-p buffer)
          (setq buffer (generate-new-buffer " *consult-jj-revision-preview*")))
        (let ((path
               (if (consult-jj-modified-file-p file)
                   (or (consult-jj-modified-file-after-path file)
                       (consult-jj-modified-file-before-path file))
                 file)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert
               (consult-jj-jj--run
                root "--ignore-working-copy"
                "file" "show" "--revision" source-rev "--" path)))
            (setq default-directory root)
            (fundamental-mode)
            (view-mode 1)
            (goto-char (point-min)))))
      (funcall preview action
               (and (eq action 'preview) file buffer))
      (when (and (memq action '(exit return)) (buffer-live-p buffer))
        (kill-buffer buffer)))))

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

(defun consult-jj--display-revision-file (contents root)
  "Display revision file CONTENTS read-only under ROOT."
  (let ((buffer (get-buffer-create consult-jj-view-file-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert contents))
      (setq default-directory (file-name-as-directory root))
      (fundamental-mode)
      (view-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

(defun consult-jj--read-revision (root)
  "Read one structured revision under ROOT."
  (consult-jj-read-commit
   (funcall consult-jj-log-function root 'default)
   "Revision: "))

(defun consult-jj--read-file-at-revision (revision root)
  "Read one file from REVISION's tree under ROOT."
  (let ((files
         (split-string
          (consult-jj-jj--run
           root "--ignore-working-copy"
           "file" "list" "--revision" revision)
          "\n" t)))
    (if files
        (completing-read "File at revision: " files nil t)
      (message "No files found at revision `%s'." revision)
      nil)))

(defun consult-jj--read-modified-file-at-revision (revision root)
  "Read one file modified by REVISION under ROOT."
  (let ((files (consult-jj-collect-files root revision)))
    (if files
        (completing-read "Modified file at revision: " files nil t)
      (message "No modified files found at revision `%s'." revision)
      nil)))

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
  (when-let* ((candidate (car (member selected candidates))))
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
  (when-let* ((candidate
              (cl-find-if
               (lambda (item)
                 (eq (get-text-property 0 'consult-jj-hunk item) hunk))
               candidates)))
    (car (consult--get-location candidate))))

(defun consult-jj-visit-hunk (hunk &optional root)
  "Visit HUNK's worktree location under ROOT when it is available.
When ROOT is nil, resolve it through `consult-jj--root'."
  (setq root (or root (consult-jj--root)))
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
