;;; consult-jj-jj.el --- Jujutsu command execution for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Internal utils for interfacing with the `jj' executable.

;;; Code:

(require 'subr-x)
(require 'consult-jj-diff)

(defcustom consult-jj-jj-executable "jj"
  "Name of, or path to, the Jujutsu executable."
  :type 'string
  :group 'consult-jj)

(defconst consult-jj-jj--global-flags '("--no-pager" "--color" "never")
  "Switches passed to every jj invocation so output is plain and parseable.")

(defun consult-jj-collect-files (root)
  "Return the list of modified files for `@' in ROOT, relative to ROOT."
  (split-string (consult-jj-jj--run root "diff" "--name-only" "-r" "@") "\n" t))

(defun consult-jj-collect-hunks (root)
  "Return the modified hunks for `@' in ROOT.
`jj diff --git' emits Git-format diffs, so parsing is delegated to
`consult-jj-diff-parse-diff'."
  (consult-jj-diff-parse-diff
   (consult-jj-jj--run root "diff" "--git" "-r" "@") root "@"))

(defun consult-jj-jj--run (root &rest args)
  "Run jj with ARGS in ROOT and return stdout as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file consult-jj-jj-executable nil t nil
                           (append consult-jj-jj--global-flags args))))
        (unless (and (integerp status) (zerop status))
          (user-error "consult-jj: jj failed: %s"
                      (string-trim (buffer-string))))
        (buffer-string)))))

(provide 'consult-jj-jj)
;;; consult-jj-jj.el ends here
