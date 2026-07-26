;;; consult-jj-git.el --- Git remote commands for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Provide commands for cloning, fetching, and pushing Git remotes through
;; Jujutsu.

;;; Code:

(require 'subr-x)
(require 'consult-jj-core)
(require 'consult-jj-jj)

(defcustom consult-jj-git-clone-dir "~/"
  "Initial parent directory offered by `consult-jj-git-clone'."
  :type 'directory
  :group 'consult-jj)

(defcustom consult-jj-git-clone-command #'project-find-file
  "Command called interactively after `consult-jj-git-clone' succeeds.
The command runs with `default-directory' set to the cloned repository.
When nil, do not run a command after cloning."
  :type '(choice (const :tag "None" nil) function)
  :group 'consult-jj)

;;;###autoload
(defun consult-jj-git-clone (&optional url parent-directory)
  "Clone Git repository URL under PARENT-DIRECTORY using Jujutsu.
Interactively, offer to use a recognized Git URL from the system clipboard,
then prompt for an existing parent directory."
  (interactive
   (list (consult-jj-git--read-clone-url)
         (read-directory-name
          "Clone into directory: "
          (expand-file-name consult-jj-git-clone-dir)
          nil t)))
  (unless (file-directory-p parent-directory)
    (user-error "consult-jj: Clone parent is not a directory: %s"
                parent-directory))
  (setq parent-directory
        (file-name-as-directory (expand-file-name parent-directory)))
  (consult-jj-jj--run parent-directory "git" "clone" url)
  (when consult-jj-git-clone-command
    (let ((default-directory
           (file-name-as-directory
            (expand-file-name
             (consult-jj-git--clone-directory-name url)
             parent-directory))))
      (call-interactively consult-jj-git-clone-command))))

;;;###autoload
(defun consult-jj-git-fetch ()
  "Fetch Git remotes for the current Jujutsu repository."
  (interactive)
  (consult-jj-jj--run (consult-jj--root) "git" "fetch")
  (message "Fetched from remote"))

;;;###autoload
(defun consult-jj-git-push ()
  "Push bookmarks from the current Jujutsu repository."
  (interactive)
  (consult-jj-jj--run (consult-jj--root) "git" "push")
  (message "Pushed to remote"))

(defun consult-jj-git--read-clone-url ()
  "Read the Git repository URL for `consult-jj-git-clone'."
  (let ((clipboard-url (consult-jj-git--clipboard-url)))
    (if (and clipboard-url
             (y-or-n-p (format "Use clipboard URL %s? " clipboard-url)))
        clipboard-url
      (read-string "Git repository URL: "))))

(defun consult-jj-git--clipboard-url ()
  "Return a recognized Git repository URL from the clipboard, or nil."
  (when-let* ((clipboard
               (condition-case nil
                   (gui-get-selection 'CLIPBOARD 'STRING)
                 (error nil)))
              ((stringp clipboard))
              (url (string-trim clipboard))
              ((string-match-p
                (concat
                 "\\`\\(?:https?://\\|ssh://\\|git://\\|"
                 "[^[:space:]@]+@[^[:space:]:]+:\\)"
                 "[^[:space:]]+\\'")
                url)))
    url))

(defun consult-jj-git--clone-directory-name (url)
  "Return the directory name that `jj git clone' derives from URL."
  (string-remove-suffix
   ".git"
   (replace-regexp-in-string
    "\\`.*[/:]" ""
    (directory-file-name url))))

(provide 'consult-jj-git)
;;; consult-jj-git.el ends here
