;;; consult-jj-workspace.el --- Jujutsu workspace workflows -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1") (consult "1.0"))

;;; Commentary:

;; Structured workspace discovery, selection, and workspace notifications.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'diff-mode)
(require 'consult)
(require 'consult-jj-core)
(require 'consult-jj-commit)
(require 'consult-jj-jj)
(require 'consult-jj-session)

(cl-defstruct (consult-jj-workspace
               (:constructor consult-jj-workspace-create)
               (:copier nil))
  "One attached Jujutsu workspace candidate.
NAME is its logical workspace name, ROOT is its absolute directory, and
WORKING-COPY is its structured working-copy commit."
  name
  root
  working-copy)

(defcustom consult-jj-workspace-preview-style 'diff
  "Preview style used by `consult-jj-read-workspace'.
The `diff' style shows the selected workspace's working-copy diff without
snapshotting it.  The `none' style disables previews."
  :type '(choice (const :tag "Diff" diff)
                 (const :tag "None" none))
  :group 'consult-jj)

(defcustom consult-jj-workspace-function #'consult-jj-collect-workspaces
  "Function used by `consult-jj-workspace-list' to collect workspaces.
The function receives the invoking project root and returns a list of
`consult-jj-workspace' objects."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-workspace-select-command #'consult-project-buffer
  "Command invoked after selecting a workspace.
The command runs interactively with `default-directory' bound to the selected
workspace root.  When nil, `consult-jj-workspace-select' returns the structured
workspace without invoking another user interface."
  :type '(choice (const :tag "Return workspace" nil) function)
  :group 'consult-jj)

(defcustom consult-jj-workspace-dir-function
  #'consult-jj-workspace-default-dir
  "Function used to propose a complete new workspace destination.
The function receives the invoking repository root and logical workspace
name."
  :type 'function
  :group 'consult-jj)

(defcustom consult-jj-use-workspace-dir-noconfirm nil
  "Whether to use proposed workspace destinations without confirmation."
  :type 'boolean
  :group 'consult-jj)

(defcustom consult-jj-workspace-sparse-patterns 'copy
  "Sparse-pattern policy used when creating a workspace.
The value is `copy', `full', or `empty'."
  :type '(choice (const copy) (const full) (const empty))
  :group 'consult-jj)

(defcustom consult-jj-new-workspace-command #'project-find-file
  "Command invoked in a newly created workspace.
When nil, do not invoke a follow-up command."
  :type '(choice (const :tag "Do nothing" nil) function)
  :group 'consult-jj)

(defcustom consult-jj-workspace-modified-hook nil
  "Hook run after Consult JJ successfully modifies workspace topology.
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
   "\",\\\"target\\\":\","
   (consult-jj-jj--commit-record-template "target")
   ", \"}\", \"\\t\", json(root), \"\\n\")")
  "Template used to serialize `jj workspace list' records as JSON lines.")

(defvar consult-jj--workspace-modified-root nil
  "Invoking workspace root for the current workspace notification.")

(defvar consult-jj-workspace--live-root nil
  "Invoking root registered by the current workspace reader, or nil.")

(defun consult-jj-collect-workspaces (root)
  "Return structured attached Jujutsu workspaces discovered under ROOT.
Discovery does not snapshot or update any working copy."
  (mapcar
   #'consult-jj-workspace--parse
   (split-string
    (consult-jj-jj--run
     root "--ignore-working-copy" "workspace" "list"
     "--template" consult-jj-workspace--list-template)
    "\n" t)))

(defun consult-jj-read-workspace (workspaces &optional prompt)
  "Read and return one structured workspace from WORKSPACES, or nil.
PROMPT defaults to `Jujutsu workspaces: '."
  (consult-jj-workspace--read
   workspaces prompt consult-jj-workspace--live-root))

(defun consult-jj-workspace--read (workspaces prompt &optional live-root)
  "Read one of WORKSPACES using PROMPT, or return nil.
When LIVE-ROOT is non-nil, register a refreshable workspace session there."
  (let ((candidates (consult-jj-workspace--candidates workspaces)))
    (when candidates
      (consult--read
       (if live-root
           (consult-jj--live-candidate-collection
            candidates live-root 'workspace nil
            #'consult-jj-workspace--collect-session-workspaces
            #'consult-jj-workspace--present-session-workspaces)
         candidates)
       :prompt (or prompt "Jujutsu workspaces: ")
       :category 'consult-jj-workspace
       :require-match t
       :sort nil
       :lookup #'consult-jj-workspace--lookup
       :history '(:input consult--line-history)
       :state (consult-jj-workspace--preview-state)))))

;;;###autoload
(defun consult-jj-workspace-select (&optional workspace)
  "Select WORKSPACE using `consult-jj-workspace-select-command'.
Interactively, read a workspace from the current project.  When the option is
nil, return WORKSPACE."
  (interactive)
  (if (null workspace)
      (consult-jj-workspace-list)
    (if consult-jj-workspace-select-command
        (let ((default-directory (consult-jj-workspace-root workspace)))
          (call-interactively consult-jj-workspace-select-command))
      workspace)))

;;;###autoload
(defun consult-jj-workspace-list ()
  "Select and visit an attached Jujutsu workspace in the current project."
  (interactive)
  (let* ((root (consult-jj--root))
         (default-directory root)
         (workspaces (funcall consult-jj-workspace-function root)))
    (if (null workspaces)
        (message "No Jujutsu workspaces found.")
      (let ((consult-jj-workspace--live-root root))
        (when-let* ((workspace (consult-jj-read-workspace workspaces)))
          (consult-jj-workspace-select workspace)))))
  nil)

;;;###autoload
(defun consult-jj-workspace-add (&optional sparse-patterns)
  "Add a Jujutsu workspace using SPARSE-PATTERNS.
Interactively, prompt for its logical name and destination.  With nil
SPARSE-PATTERNS, use `consult-jj-workspace-sparse-patterns'."
  (interactive)
  (let* ((policy (or sparse-patterns
                     consult-jj-workspace-sparse-patterns))
         (_
          (unless (memq policy '(copy full empty))
            (user-error
             "consult-jj: Invalid workspace sparse-pattern policy `%s'"
             policy)))
         (root (file-name-as-directory
                (expand-file-name (consult-jj--root))))
         (workspace-name (read-string "Workspace name: "))
         (proposal
          (file-name-as-directory
           (expand-file-name
            (funcall consult-jj-workspace-dir-function
                     root workspace-name))))
         (destination
          (if consult-jj-use-workspace-dir-noconfirm
              proposal
            (file-name-as-directory
             (expand-file-name
              (read-directory-name
               "Workspace directory: " nil proposal nil proposal))))))
    (make-directory
     (file-name-directory (directory-file-name destination)) t)
    (consult-jj-jj--run
     root "workspace" "add"
     "--name" workspace-name
     "--sparse-patterns" (symbol-name policy)
     destination)
    (let ((consult-jj--workspace-modified-root root)
          (consult-jj--commit-modified-root root)
          (consult-jj--candidate-refresh-context (list nil)))
      (run-hooks 'consult-jj-workspace-modified-hook)
      (run-hooks 'consult-jj-commit-modified-hook))
    (when consult-jj-new-workspace-command
      (let ((default-directory destination))
        (call-interactively consult-jj-new-workspace-command))))
  nil)

(defun consult-jj-workspace-default-dir (root workspace-name)
  "Return the default destination for WORKSPACE-NAME created under ROOT.
Use the repository-host workspace's directory name when its shared Jujutsu
store can be resolved structurally."
  (let* ((host-root
          (or (consult-jj-workspace--repository-host-root root)
              (file-name-as-directory (expand-file-name root))))
         (project-name
          (file-name-nondirectory (directory-file-name host-root))))
    (file-name-as-directory
     (expand-file-name
      workspace-name
      (expand-file-name project-name "~/jj-workspaces/")))))

(defun consult-jj-workspace--repository-host-root (root)
  "Return the repository-host workspace root discovered from ROOT, or nil."
  (let* ((metadata-root (expand-file-name ".jj" root))
         (repository-entry (expand-file-name "repo" metadata-root))
         (repository-dir
          (cond
           ((file-directory-p repository-entry)
            repository-entry)
           ((file-readable-p repository-entry)
            (with-temp-buffer
              (insert-file-contents repository-entry)
              (expand-file-name
               (string-trim (buffer-string)) metadata-root))))))
    (when repository-dir
      (setq repository-dir
            (directory-file-name (file-truename repository-dir)))
      (let ((host-metadata-root
             (file-name-directory repository-dir)))
        (when (and
               (file-directory-p repository-dir)
               (equal (file-name-nondirectory repository-dir) "repo")
               (equal
                (file-name-nondirectory
                 (directory-file-name host-metadata-root))
                ".jj"))
          (file-name-directory
           (directory-file-name host-metadata-root)))))))

(defun consult-jj-workspace--refresh-live-candidate-sessions ()
  "Refresh live sessions under `consult-jj--workspace-modified-root'."
  (when consult-jj--workspace-modified-root
    (consult-jj--refresh-candidate-sessions-once
     (file-name-as-directory
      (expand-file-name consult-jj--workspace-modified-root)))))

(defun consult-jj-workspace--collect-session-workspaces (root _tier)
  "Collect structured workspace objects under ROOT for a live session."
  (funcall consult-jj-workspace-function root))

(defun consult-jj-workspace--present-session-workspaces (workspaces _root)
  "Present WORKSPACES as name-only completion candidates."
  (consult-jj-workspace--candidates workspaces))

(defun consult-jj-workspace--preview-state ()
  "Return the preview state selected by `consult-jj-workspace-preview-style'."
  (pcase consult-jj-workspace-preview-style
    ('diff (consult-jj-workspace--diff-preview-state))
    ('none nil)
    (style
     (user-error "consult-jj: Invalid workspace preview style `%s'" style))))

(defun consult-jj-workspace--diff-preview-state ()
  "Return a transient diff preview state for workspace candidates."
  (let ((preview (consult--buffer-preview))
        buffer)
    (lambda (action workspace)
      (when (and (eq action 'preview) workspace)
        (unless (buffer-live-p buffer)
          (setq buffer
                (generate-new-buffer
                 consult-jj-workspace--preview-buffer-name)))
        (condition-case error-data
            (consult-jj-workspace--render-diff
             (consult-jj-workspace--diff workspace)
             buffer)
          (error
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (signal (car error-data) (cdr error-data)))))
      (funcall preview action
               (and (eq action 'preview) workspace buffer))
      (when (and (memq action '(exit return)) (buffer-live-p buffer))
        (kill-buffer buffer)))))

(defun consult-jj-workspace--diff (workspace)
  "Return the working-copy diff for WORKSPACE without snapshotting."
  (consult-jj-jj--run
   (consult-jj-workspace-root workspace)
   "--ignore-working-copy" "diff" "--git" "--revision"
   (consult-jj-commit-commit-id
    (consult-jj-workspace-working-copy workspace))))

(defun consult-jj-workspace--render-diff (diff buffer)
  "Render DIFF in BUFFER and return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert diff))
    (diff-mode)
    (goto-char (point-min)))
  buffer)

(defun consult-jj-workspace--candidates (workspaces)
  "Build name-only completion candidates for WORKSPACES in source order."
  (cl-loop for workspace in workspaces
           for index from 0
           collect
           (let ((candidate
                  (consult--tofu-append
                   (consult-jj-workspace-name workspace) index)))
             (add-text-properties
              0 1 (list 'consult-jj-workspace workspace) candidate)
             candidate)))

(defun consult-jj-workspace--lookup (selected candidates &rest _)
  "Return the workspace object for SELECTED from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-workspace candidate)))

(defun consult-jj-workspace--parse (line)
  "Parse one JSON workspace record from LINE."
  (pcase-let* ((`(,record-json ,root-json)
                (split-string line "\t"))
               (record
                (json-parse-string record-json :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil))
               (name (alist-get 'name record))
               (_
                (when (or (null root-json)
                          (string-prefix-p "<Error:" root-json))
                  (user-error
                   "consult-jj: Workspace `%s' has no recorded root"
                   name)))
               (root (json-parse-string root-json))
               (target (alist-get 'target record))
               (working-copy
                (consult-jj-jj--commit-from-record target))
               (_
                (unless (file-name-absolute-p root)
                  (user-error
                   "consult-jj: Workspace `%s' has no recorded root"
                   name))))
    (unless
        (member
         name
         (consult-jj-commit-working-copy-workspaces working-copy))
      (setf
       (consult-jj-commit-working-copy-workspaces working-copy)
       (append
        (consult-jj-commit-working-copy-workspaces working-copy)
        (list name))))
    (consult-jj-workspace-create
     :name name
     :root (file-name-as-directory
            (expand-file-name root))
     :working-copy working-copy)))

(consult-jj--register-candidate-session-adapter
 'workspace
 #'consult-jj-workspace--collect-session-workspaces
 #'consult-jj-workspace--present-session-workspaces)

(add-hook 'consult-jj-workspace-modified-hook
          #'consult-jj-workspace--refresh-live-candidate-sessions)

(provide 'consult-jj-workspace)
;;; consult-jj-workspace.el ends here
