;;; consult-jj-jj-test.el --- Tests for Jujutsu collection -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for collecting hunks from the Jujutsu working-copy commit.

;;; Code:

(require 'ert)
(require 'consult-jj-jj)

(ert-deftest consult-jj-collect-hunks-uses-current-revision ()
  (let ((diff "diff --git a/file.txt b/file.txt
index 1111111..2222222 100644
--- a/file.txt
+++ b/file.txt
@@ -1 +1 @@
-before
+after
")
        call)
    (cl-letf (((symbol-function 'consult-jj-jj--run)
               (lambda (root &rest args)
                 (setq call (cons root args))
                 diff)))
      (let ((hunk (car (consult-jj-collect-hunks "/repo/"))))
        (should (equal call '("/repo/" "diff" "--git" "-r" "@")))
        (should (consult-jj-hunk-p hunk))
        (should (equal (consult-jj-hunk-root hunk) "/repo/"))
        (should (equal (consult-jj-hunk-source-rev hunk) "@"))))))

(provide 'consult-jj-jj-test)
;;; consult-jj-jj-test.el ends here
