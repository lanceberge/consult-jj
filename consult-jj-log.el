;;; consult-jj-log.el --- Commit-log discovery for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Structured commit-log discovery, ordered revset tiers, and commit selection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'diff-mode)
(require 'consult)
(require 'consult-jj-commit)
(require 'consult-jj-jj)
(require 'consult-jj-session)

(declare-function consult-jj-view-diff-in-revision "consult-jj")

(defcustom consult-jj-commits-for-file-command #'project-find-file
  "Command called interactively to visit a file for its commit history."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-commits-for-file-preview-style 'diff
  "Preview style for commits that modified one file."
  :type '(choice (const :tag "File diff" diff)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-log-preview-style 'diff
  "Preview style used by `consult-jj-read-commit'.
The `diff' style shows the selected commit and its patch.  The `none' style
disables preview."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-change-id-style 'unique
  "Change ID style used only in commit candidate presentation.
The `unique' style shows Jujutsu's unique change ID prefix.  The `full' style
also shows the remainder of Jujutsu's normal abbreviated display."
  :type '(choice (const :tag "Unique prefix" unique)
                 (const :tag "Full abbreviated display" full))
  :group 'consult-jj)

(defface consult-jj-change-id-unique
  '((t :inherit font-lock-keyword-face))
  "Face for Jujutsu's unique change ID prefix."
  :group 'consult-jj)

(defface consult-jj-change-id-remainder
  '((t :inherit shadow))
  "Face for the remainder of Jujutsu's abbreviated change ID."
  :group 'consult-jj)

(make-obsolete-variable 'consult-jj-log-preview
                        'consult-jj-log-preview-style "0.2.0")

(defcustom consult-jj-log-function #'consult-jj-collect-commits
  "Function used to collect structured Commit candidates.
The function receives the repository root and selected revset, then
returns a list of `consult-jj-commit' objects."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-log-revsets
  '(default
    "present(@) | ancestors(immutable_heads().., 10) | trunk()"
    "present(@) | ancestors(immutable_heads().., 50) | trunk()"
    "present(@) | ancestors(immutable_heads().., 100) | trunk()"
    "all()")
  "Ordered revset tiers used by `consult-jj-log'.
The `default' entry uses Jujutsu's configured `revsets.log'.
String entries are passed as explicit revsets.  Entries
should be ordered from narrowest to broadest."
  :type '(repeat
          (choice (const :tag "Configured default" default)
                  (string :tag "Explicit revset")))
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

(defconst consult-jj--log-preview-buffer-name "*consult-jj-log-preview*"
  "Base name of the temporary commit preview buffer.")

(cl-defstruct (consult-jj-file-history-entry
               (:constructor consult-jj-file-history-entry-create)
               (:copier nil))
  "A structured commit and repository-relative file in its history."
  commit file)

(defvar consult-jj--last-log-tier nil
  "Most recent revset tier used by a commit-log session.")

(defvar consult-jj-log-minibuffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-+") #'consult-jj-log-expand)
    (define-key map (kbd "M--") #'consult-jj-log-shrink)
    map)
  "Keymap active in `consult-jj-log' minibuffers.")

(define-minor-mode consult-jj-log-minibuffer-mode
  "Enable commit-log tier commands in the current minibuffer."
  :init-value nil
  :lighter nil
  :keymap consult-jj-log-minibuffer-mode-map)

;;;###autoload
(define-minor-mode consult-jj-commit-two-line-mode
  "Present commit selector candidates on two physical rows.
Toggling the global mode does not refresh an already open commit selector.
The new presentation takes effect on the next commit read or ordinary
candidate refresh."
  :global t
  :init-value nil
  :group 'consult-jj)

;;;###autoload
(defun consult-jj-log (&optional prefix)
  "Select and visit a Jujutsu commit from the current project's log.
With a universal PREFIX, start at the broadest configured revset tier.
Numeric prefixes have no meaning and signal a `user-error'."
  (interactive "P")
  (when (integerp prefix)
    (user-error "consult-jj: Numeric prefixes are not valid log tiers"))
  (let* ((root (consult-jj--root))
         (default-directory root)
         (revset
          (cond
           ((or (eq prefix 'default) (stringp prefix)) prefix)
           ((consp prefix) (car (last consult-jj-log-revsets)))
           (t (car consult-jj-log-revsets))))
         (commits
          (funcall consult-jj-log-function root revset)))
    (setq consult-jj--last-log-tier revset)
    (if (null commits)
        (message "No Jujutsu commits found.")
      (when-let* ((selected
                  (consult-jj--read-commit commits nil root nil revset)))
        (funcall consult-jj-log-visit-function
                 (consult-jj--commit-id selected)))))
  nil)

;;;###autoload
(defun consult-jj-commits-for-file (&optional file prefix)
  "Browse commits modifying FILE in the invoking Jujutsu repository.
Interactively, visit a file with `consult-jj-commits-for-file-command'.
With a universal PREFIX, use the current buffer's visited file instead."
  (interactive (list nil current-prefix-arg))
  (when (integerp prefix)
    (user-error "consult-jj: Numeric prefixes are not valid for file history"))
  (let* ((root (file-name-as-directory (expand-file-name (consult-jj--root))))
         (default-directory root)
         (chosen
          (or file
              (if prefix
                  (or buffer-file-name
                      (user-error "consult-jj: Current buffer does not visit a file"))
                (progn
                  (call-interactively consult-jj-commits-for-file-command)
                  (or buffer-file-name
                      (user-error "consult-jj: File chooser did not visit a file"))))))
         (absolute (expand-file-name chosen))
         (_inside
          (unless (file-in-directory-p absolute root)
            (user-error "consult-jj: File is outside the invoking repository: %s"
                        chosen)))
         (relative (file-relative-name absolute root))
         (commits (consult-jj-log--collect-file-history root nil relative)))
    (if (null commits)
        (message "No commits modified %s" relative)
      (when-let* ((selected (consult-jj--read-file-history
                             commits relative root)))
        (let ((default-directory root))
          (consult-jj-view-diff-in-revision
           (consult-jj--commit-id
            (consult-jj-file-history-entry-commit selected))
           (consult-jj-file-history-entry-file selected))))))
  nil)

;;;###autoload
(defun consult-jj-log-expand ()
  "Expand the active or replacing commit log by one revset tier."
  (interactive)
  (consult-jj-log--move-tier 1))

;;;###autoload
(defun consult-jj-log-shrink ()
  "Shrink the active or replacing commit log by one revset tier."
  (interactive)
  (consult-jj-log--move-tier -1))

(defun consult-jj-read-commit (commits &optional prompt default)
  "Read and return one structured commit from COMMITS, or nil.
COMMITS must contain `consult-jj-commit' objects.  Completion candidates show
captured compact topology followed by the first description line.  Commits
without captured topology remain flat.  PROMPT defaults to
`Jujutsu commits: '.  DEFAULT, when non-nil, is the commit offered as the
default candidate."
  (consult-jj--read-commit commits prompt nil default))

(defun consult-jj--read-file-history (commits file root)
  "Read a file-history entry for FILE from COMMITS under ROOT."
  (let ((candidates (consult-jj-log--file-history-candidates commits file))
        (default-directory root))
    (consult--read
     (consult-jj--live-candidate-collection
      candidates root 'file-history nil
      #'consult-jj-log--collect-file-history
      (lambda (refreshed _root)
        (consult-jj-log--file-history-candidates refreshed file))
      file)
     :prompt (format "Commits for %s: " file)
     :category 'consult-jj-file-history-entry
     :require-match t
     :sort nil
     :lookup #'consult-jj-log--lookup-file-history
     :history '(:input consult--line-history)
     :state (consult-jj-log--file-preview-state))))

(defun consult-jj-default-log-visit (commit-id)
  "Display COMMIT-ID and its diff in a persistent Consult JJ buffer."
  (consult-jj-log--display-commit commit-id))

(defun consult-jj-log--move-tier (step)
  "Move the current commit-log tier by STEP, which must be 1 or -1."
  (let* ((session
          (cl-find-if
           (lambda (candidate-session)
             (and
              (eq (consult-jj--candidate-session-view candidate-session) 'log)
              (eq (consult-jj--candidate-session-buffer candidate-session)
                  (current-buffer))))
           consult-jj--candidate-sessions))
         (current
          (if session
              (consult-jj--candidate-session-tier session)
            consult-jj--last-log-tier))
         (tiers (if (= step 1)
                    consult-jj-log-revsets
                  (reverse consult-jj-log-revsets)))
         (remaining (and current (member current tiers)))
         (next (cadr remaining)))
    (unless current
      (user-error "consult-jj: No commit-log tier to %s"
                  (if (= step 1) "expand" "shrink")))
    (if (null next)
        (message "consult-jj: Commit log is already at the %s tier"
                 (if (= step 1) "broadest" "narrowest"))
      (setq consult-jj--last-log-tier next)
      (if session
          (progn
            (setf (consult-jj--candidate-session-tier session) next)
            (funcall
             (consult-jj--candidate-session-replace session)
             (consult-jj--commit-candidates
              (funcall consult-jj-log-function
                       (consult-jj--candidate-session-root session)
                       next))))
        (consult-jj-log next))))
  nil)

(defun consult-jj--read-commit
    (commits prompt &optional live-root default revset)
  "Read one of COMMITS using PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable log session there.
DEFAULT, when non-nil, is the commit offered as the default candidate.
REVSET is the active tier retained by a live session."
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
      (let ((read
             (lambda ()
               (consult--read
                (if live-root
                    (consult-jj--live-candidate-collection
                     candidates live-root 'log revset
                     #'consult-jj-log--collect-session-commits
                     #'consult-jj-log--present-session-commits)
                  candidates)
                :prompt (or prompt "Jujutsu commits: ")
                :category 'consult-jj-commit
                :require-match t
                :sort nil
                :lookup #'consult-jj--lookup-commit
                :default default-candidate
                :history '(:input consult--line-history)
                :state state))))
        (if live-root
            (minibuffer-with-setup-hook
                #'consult-jj-log-minibuffer-mode
              (funcall read))
          (funcall read))))))

(defun consult-jj--commit-candidate (commit index)
  "Build a completion candidate for COMMIT disambiguated by INDEX."
  (let* ((description-display (consult-jj-log--description commit))
         (one-line-display
          (concat (or (consult-jj-commit-graph-prefix commit) "")
                  description-display))
         (candidate
          (consult--tofu-append
           (if consult-jj-commit-two-line-mode
               description-display
             one-line-display)
           index))
         (two-line-display
          (and
           consult-jj-commit-two-line-mode
           (concat
            (or (consult-jj-commit-two-line-graph-prefix commit) "")
            (consult-jj--commit-revision-identity commit)
            "\n"
            (or (consult-jj-commit-two-line-graph-continuation commit) "")
            description-display))))
    (add-text-properties 0 1 (list 'consult-jj-commit commit) candidate)
    (when two-line-display
      (add-text-properties
       0 (1- (length candidate))
       (list 'display two-line-display
             'consult-jj-two-line-display two-line-display)
       candidate))
    candidate))

(defun consult-jj-log--description (commit)
  "Return COMMIT's first description line for a completion candidate."
  (let ((first-line
         (car (split-string (consult-jj-commit-description commit) "\n"))))
    (if (string-empty-p (or first-line ""))
        "(no description set)"
      first-line)))

(defun consult-jj--commit-revision-identity (commit)
  "Return COMMIT's styled contextual identity for candidate presentation."
  (cond
   ((consult-jj-commit-current-p commit) "@")
   ((consult-jj-commit-parent-p commit) "@-")
   (t (consult-jj--commit-change-id commit))))

(defun consult-jj--commit-change-id (commit)
  "Return COMMIT's styled candidate-only change ID."
  (let* ((unique
          (or (consult-jj-commit-change-id-unique commit)
              (consult-jj-commit-short-change-id commit)
              ""))
         (remainder
          (or (consult-jj-commit-change-id-remainder commit) ""))
         (offset
          (if (and (consult-jj-commit-divergent-p commit)
                   (numberp (consult-jj-commit-change-offset commit)))
              (format "/%d" (consult-jj-commit-change-offset commit))
            "")))
    (concat
     (propertize unique 'face 'consult-jj-change-id-unique)
     (if (eq consult-jj-change-id-style 'full)
         (propertize remainder 'face 'consult-jj-change-id-remainder)
       "")
     (propertize offset 'face 'consult-jj-change-id-unique))))

(defun consult-jj-log--collect-session-commits (root revset _source-rev)
  "Collect commit objects under ROOT for REVSET."
  (funcall consult-jj-log-function root (or revset 'default)))

(defun consult-jj-log--collect-file-history (root _tier file)
  "Collect COMMITS under ROOT whose introduced change modifies FILE."
  (let* ((fileset (car (consult-jj-jj--exact-filesets (list file) root)))
         (revset (format "all() & files(%s)" fileset)))
    (funcall consult-jj-log-function root revset)))

(defun consult-jj-log--file-history-candidates (commits file)
  "Build file-history candidates from COMMITS and FILE in source order."
  (cl-loop for commit in commits
           for index from 0
           for entry = (consult-jj-file-history-entry-create
                        :commit commit :file file)
           for candidate = (consult--tofu-append
                            (consult-jj-log--description commit) index)
           do (add-text-properties
               0 1 (list 'consult-jj-file-history-entry entry) candidate)
           collect candidate))

(defun consult-jj-log--lookup-file-history (selected candidates &rest _)
  "Return SELECTED's file-history entry from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-file-history-entry candidate)))

(defun consult-jj-log--present-session-commits (commits _root)
  "Present COMMITS as completion candidates."
  (consult-jj--commit-candidates commits))

(consult-jj--register-candidate-session-adapter
 'log
 #'consult-jj-log--collect-session-commits
 #'consult-jj-log--present-session-commits)

(defun consult-jj--commit-candidates (commits)
  "Build completion candidates for structured COMMITS in source order."
  (cl-loop for commit in commits
           for index from 0
           collect (consult-jj--commit-candidate commit index)))

(defun consult-jj--lookup-commit (selected candidates &rest _)
  "Return the commit object for SELECTED from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-commit candidate)))

(defun consult-jj--log-preview-state ()
  "Return the preview state selected by `consult-jj-log-preview-style'."
  (pcase consult-jj-log-preview-style
    ('diff
     (consult-jj-log--diff-preview-state))
    ('none nil)
    (style (user-error "consult-jj: Invalid log preview style `%s'" style))))

(defun consult-jj-log--file-preview-state ()
  "Return a transient preview state for file-history entries."
  (pcase consult-jj-commits-for-file-preview-style
    ('none nil)
    ('diff
     (consult-jj-log--diff-preview-state
      (lambda (entry)
        (consult-jj-jj--diff-files
         (list (consult-jj-file-history-entry-file entry))
         default-directory
         (consult-jj--commit-id
          (consult-jj-file-history-entry-commit entry))))))
    (style
     (user-error "consult-jj: Invalid file-history preview style `%s'" style))))

(defun consult-jj-log--diff-preview-state (&optional diff-function)
  "Return a transient preview state rendering each candidate's diff.
DIFF-FUNCTION overrides the ordinary commit diff used by log and tag readers."
  (let ((preview (consult--buffer-preview))
        (diff-function (or diff-function
                           (lambda (commit)
                             (consult-jj-log--commit-diff
                              (consult-jj--commit-id commit)
                              default-directory))))
        buffer)
    (lambda (action candidate)
      (when (and (eq action 'preview) candidate)
        (unless (buffer-live-p buffer)
          (setq buffer
                (generate-new-buffer consult-jj--log-preview-buffer-name)))
        (consult-jj-log--render-diff
         (funcall diff-function candidate)
         buffer))
      (funcall preview action
               (and (eq action 'preview) candidate buffer))
      (when (and (memq action '(exit return)) (buffer-live-p buffer))
        (kill-buffer buffer)))))

(defun consult-jj-log--display-commit (commit-id)
  "Render COMMIT-ID into the persistent log display buffer."
  (let ((root default-directory)
        (buffer (get-buffer-create consult-jj-log-buffer-name)))
    (with-current-buffer buffer
      (setq default-directory root))
    (consult-jj-log--render-diff
     (consult-jj-log--commit-diff commit-id root)
     buffer)
    (display-buffer buffer)
    buffer))

(defun consult-jj-log--render-diff (diff buffer)
  "Render DIFF in BUFFER and return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert diff))
    (diff-mode)
    (goto-char (point-min)))
  buffer)

(defun consult-jj-log--commit-diff (commit-id root)
  "Return the Git-format diff presentation of COMMIT-ID under ROOT."
  (consult-jj-jj--run root "show" "--git" commit-id))

(provide 'consult-jj-log)
;;; consult-jj-log.el ends here
