;;; consult-jj-git.el --- Git remote commands for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Provide commands for fetching and pushing Git remotes through Jujutsu.

;;; Code:

(require 'consult-jj-core)
(require 'consult-jj-jj)

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

(provide 'consult-jj-git)
;;; consult-jj-git.el ends here
