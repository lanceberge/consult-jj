;;; consult-vc-jj.el --- Jujutsu provider for consult-vc -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Code:

(require 'subr-x)
(require 'consult-vc-provider)
(require 'consult-vc-git)

(defcustom consult-vc-jj-executable "jj"
  "Name of, or path to, the Jujutsu executable."
  :type 'string
  :group 'consult-vc)

(defconst consult-vc-jj--global-switches '("--no-pager" "--color" "never")
  "Switches passed to every jj invocation so output is plain and parseable.")

(defun consult-vc-jj--run (root &rest args)
  "Run jj with ARGS in ROOT and return stdout as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file consult-vc-jj-executable nil t nil
                           (append consult-vc-jj--global-switches args))))
        (unless (and (integerp status) (zerop status))
          (user-error "consult-vc: jj failed: %s"
                      (string-trim (buffer-string))))
        (buffer-string)))))

(defun consult-vc-jj-collect-files (root)
  "Return the list of modified files for `@' in ROOT, relative to ROOT."
  (split-string (consult-vc-jj--run root "diff" "--name-only" "-r" "@") "\n" t))

(defun consult-vc-jj-collect-hunks (root)
  "Return the provider-neutral hunks for `@' in ROOT.
`jj diff --git' emits Git-format diffs, so parsing is delegated to
`consult-vc-git-parse-diff'."
  (consult-vc-git-parse-diff
   (consult-vc-jj--run root "diff" "--git" "-r" "@")
   'jj root "@"))

(consult-vc-provider-register
 (consult-vc-provider-def-create
  :name 'jj
  :collect-files #'consult-vc-jj-collect-files
  :collect-hunks #'consult-vc-jj-collect-hunks))

(provide 'consult-vc-jj)
;;; consult-vc-jj.el ends here
