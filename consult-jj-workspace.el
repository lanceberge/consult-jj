;;; consult-jj-workspace.el --- Workspace workflows for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Structured Jujutsu workspace discovery and selection.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'diff-mode)
(require 'consult)
(require 'consult-jj-core)
(require 'consult-jj-commit)
(require 'consult-jj-jj)
(require 'consult-jj-session)

(cl-defstruct (consult-jj-workspace
               (:constructor consult-jj-workspace-create)
               (:copier nil))
  "One attached Jujutsu workspace candidate."
  name root working-copy)

(defcustom consult-jj-workspace-preview-style 'diff
  "Preview style used by `consult-jj-read-workspace'.
The `diff' style shows the selected workspace's working-copy diff.  The
`none' style disables previews."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-workspace-select-command #'consult-project-buffer
  "Command invoked after selecting a workspace.
The command runs interactively with `default-directory' bound to the selected
workspace root.  When nil, `consult-jj-workspace-select' returns the structured
workspace without invoking another interface."
  :type '(choice (const :tag "Return workspace" nil)
                 (function :tag "Interactive command"))
  :group 'consult-jj)

(defcustom consult-jj-workspace-function #'consult-jj-collect-workspaces
  "Function used by `consult-jj-workspace-list' to collect workspaces.
The function receives the invoking project root and returns a list of
`consult-jj-workspace' objects."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-workspace-modified-hook nil
  "Hook run after Consult JJ successfully changes workspace state.
The hook runs with no arguments."
  :type 'hook
  :group 'consult-jj)

(defconst consult-jj-workspace--preview-buffer-name
  "*consult-jj-workspace-preview*"
  "Base name of the temporary workspace preview buffer.")

(defconst consult-jj-workspace--list-template
  (concat
   "concat("
   "\"{\\\"name\\\":\", stringify(name).escape_json(),"
   "\",\\\"root\\\":\\\"\","
   "stringify(root).escape_json().substr(1, -1),"
   "\"\\\",\\\"target\\\":{\\\"change_id\\\":\","
   "stringify(target.change_id()).escape_json(),"
   "\",\\\"commit_id\\\":\", stringify(target.commit_id()).escape_json(),"
   "\",\\\"description\\\":\", target.description().escape_json(),"
   "\",\\\"author\\\":{\\\"name\\\":\","
   "target.author().name().escape_json(),"
   "\",\\\"email\\\":\","
   "stringify(target.author().email()).escape_json(),"
   "\",\\\"timestamp\\\":\","
   "target.author().timestamp().format(\"%+\").escape_json(), \"}\","
   "\",\\\"bookmarks\\\":\","
   "stringify(target.local_bookmarks().map(|b| b.name()).join(\"\\0\"))"
   ".escape_json(),"
   "\",\\\"current\\\":\", target.current_working_copy(),"
   "\",\\\"parent\\\":\", target.contained_in(\"@-\"), \"}}\\n\")")
  "Template used to serialize `jj workspace list' records.")

(defvar consult-jj--workspace-modified-root nil
  "Invoking workspace root for the current workspace notification.")

;;;###autoload
(defun consult-jj-workspace-list ()
  "Select and visit an attached workspace from the current project."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (workspaces (funcall consult-jj-workspace-function root)))
    (if (null workspaces)
        (message "No Jujutsu workspaces found.")
      (when-let ((workspace
                  (consult-jj-workspace--read workspaces nil root)))
        (consult-jj-workspace-select workspace))))
  nil)

(defun consult-jj-collect-workspaces (root)
  "Return structured attached Jujutsu workspaces discovered from ROOT."
  (delq
   nil
   (mapcar
    #'consult-jj-workspace--parse
    (split-string
     (consult-jj-jj--run
      root "workspace" "list" "--ignore-working-copy"
      "--template" consult-jj-workspace--list-template)
     "\n" t))))

(defun consult-jj-read-workspace (workspaces &optional prompt)
  "Read and return one structured workspace from WORKSPACES, or nil.
Completion candidates display workspace names only.  PROMPT defaults to
`Jujutsu workspaces: '."
  (consult-jj-workspace--read workspaces prompt nil))

(defun consult-jj-workspace-select (workspace)
  "Select WORKSPACE using `consult-jj-workspace-select-command'.
When that option is nil, return WORKSPACE."
  (if consult-jj-workspace-select-command
      (let ((default-directory (consult-jj-workspace-root workspace)))
        (call-interactively consult-jj-workspace-select-command))
    workspace))

(defun consult-jj-workspace--read (workspaces prompt live-root)
  "Read one of WORKSPACES with PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable workspace session there."
  (let ((candidates (consult-jj-workspace--candidates workspaces)))
    (when candidates
      (consult--read
       (if live-root
           (consult-jj--live-candidate-collection
            candidates live-root 'workspace nil
            #'consult-jj-workspace--collect-session
            #'consult-jj-workspace--present-session)
         candidates)
       :prompt (or prompt "Jujutsu workspaces: ")
       :category 'consult-jj-workspace
       :require-match t
       :sort nil
       :lookup #'consult-jj-workspace--lookup
       :history '(:input consult--line-history)
       :state (consult-jj-workspace--preview-state)))))

(defun consult-jj-workspace--collect-session (root _tier)
  "Collect workspace objects under ROOT for a live session."
  (funcall consult-jj-workspace-function root))

(defun consult-jj-workspace--present-session (workspaces _root)
  "Present WORKSPACES as completion candidates."
  (consult-jj-workspace--candidates workspaces))

(consult-jj--register-candidate-session-adapter
 'workspace
 #'consult-jj-workspace--collect-session
 #'consult-jj-workspace--present-session)

(defun consult-jj-workspace--refresh-live-candidate-sessions ()
  "Refresh live sessions for `consult-jj--workspace-modified-root'."
  (when consult-jj--workspace-modified-root
    (consult-jj--refresh-candidate-sessions
     (file-name-as-directory
      (expand-file-name consult-jj--workspace-modified-root)))))

(defun consult-jj-workspace--candidates (workspaces)
  "Build name-only completion candidates for WORKSPACES."
  (cl-loop
   for workspace in workspaces
   for index from 0
   for candidate =
   (consult--tofu-append (consult-jj-workspace-name workspace) index)
   do (add-text-properties
       0 1 (list 'consult-jj-workspace workspace) candidate)
   collect candidate))

(defun consult-jj-workspace--lookup (selected candidates &rest _)
  "Return the workspace carried by SELECTED from CANDIDATES."
  (when-let ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-workspace candidate)))

(defun consult-jj-workspace--preview-state ()
  "Return the state function for `consult-jj-workspace-preview-style'."
  (pcase consult-jj-workspace-preview-style
    ('none nil)
    ('diff (consult-jj-workspace--diff-preview-state))
    (style
     (user-error "consult-jj: Invalid workspace preview style `%s'" style))))

(defun consult-jj-workspace--diff-preview-state ()
  "Return a transient diff preview state for workspace candidates."
  (let ((preview (consult--buffer-preview))
        buffer)
    (lambda (action workspace)
      (condition-case err
          (progn
            (when (and (eq action 'preview) workspace)
              (unless (buffer-live-p buffer)
                (setq buffer
                      (generate-new-buffer
                       consult-jj-workspace--preview-buffer-name)))
              (consult-jj-workspace--render-diff
               (consult-jj-jj--run
                (consult-jj-workspace-root workspace)
                "diff" "--git" "--ignore-working-copy" "--revision"
                (consult-jj-commit-commit-id
                 (consult-jj-workspace-working-copy workspace)))
               buffer))
            (funcall preview action
                     (and (eq action 'preview) workspace buffer))
            (when (and (memq action '(exit return))
                       (buffer-live-p buffer))
              (kill-buffer buffer)))
        ((error quit)
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         (signal (car err) (cdr err)))))))

(defun consult-jj-workspace--render-diff (diff buffer)
  "Render DIFF in BUFFER and return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert diff))
    (diff-mode)
    (goto-char (point-min)))
  buffer)

(defun consult-jj-workspace--parse (line)
  "Parse one JSON workspace record from LINE."
  (let* ((record
          (json-parse-string line :object-type 'alist
                            :array-type 'list
                            :null-object nil
                            :false-object nil))
         (target (alist-get 'target record))
         (author (alist-get 'author target))
         (bookmarks (alist-get 'bookmarks target))
         (root (alist-get 'root record)))
    (when (file-name-absolute-p root)
      (consult-jj-workspace-create
       :name (alist-get 'name record)
       :root (file-name-as-directory (expand-file-name root))
       :working-copy
       (consult-jj-commit-create
        :change-id (alist-get 'change_id target)
        :commit-id (alist-get 'commit_id target)
        :description (alist-get 'description target)
        :author-name (alist-get 'name author)
        :author-email (alist-get 'email author)
        :timestamp (alist-get 'timestamp author)
        :bookmarks (split-string (or bookmarks "") "\0" t)
        :current-p (alist-get 'current target)
        :parent-p (alist-get 'parent target))))))

(add-hook 'consult-jj-workspace-modified-hook
          #'consult-jj-workspace--refresh-live-candidate-sessions)

(provide 'consult-jj-workspace)
;;; consult-jj-workspace.el ends here
