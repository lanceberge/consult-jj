;;; consult-vc-jj-test.el --- Tests for the Jujutsu provider -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for collecting provider-neutral hunks from the current jj revision.

;;; Code:

(require 'ert)
(require 'consult-vc-jj)

(ert-deftest consult-vc-jj-collect-hunks-uses-current-revision ()
  (let ((diff "diff --git a/file.txt b/file.txt
index 1111111..2222222 100644
--- a/file.txt
+++ b/file.txt
@@ -1 +1 @@
-before
+after
")
        call)
    (cl-letf (((symbol-function 'consult-vc-jj--run)
               (lambda (root &rest args)
                 (setq call (cons root args))
                 diff)))
      (let ((hunk (car (consult-vc-jj-collect-hunks "/repo/"))))
        (should (equal call '("/repo/" "diff" "--git" "-r" "@")))
        (should (consult-vc-hunk-p hunk))
        (should (eq (consult-vc-hunk-provider hunk) 'jj))
        (should (equal (consult-vc-hunk-root hunk) "/repo/"))
        (should (equal (consult-vc-hunk-source-rev hunk) "@"))))))

(ert-deftest consult-vc-jj-run-reports-command-failure ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _)
               (insert "repository error\n")
               1)))
    (should-error (consult-vc-jj--run "/tmp/" "diff") :type 'user-error)))

(provide 'consult-vc-jj-test)
;;; consult-vc-jj-test.el ends here
