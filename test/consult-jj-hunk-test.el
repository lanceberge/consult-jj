;;; consult-jj-hunk-test.el --- Tests for the Consult JJ hunk model -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'ert)
(require 'consult-jj-hunk)

(defun consult-jj-hunk-test--modified ()
  "Return a small supported `modified' hunk built by hand."
  (consult-jj-hunk-create
   :root "/r" :source-rev "@"
   :old-path "mod.txt" :new-path "mod.txt" :status 'modified
   :file-header "diff --git a/mod.txt b/mod.txt
index b3c5a95..ef75112 100644
--- a/mod.txt
+++ b/mod.txt"
   :hunk-header "@@ -1,3 +1,3 @@ ctx"
   :lines (list (consult-jj-hunk-line-create
                 :type 'context :text "line1" :old-lineno 1 :new-lineno 1)
                (consult-jj-hunk-line-create
                 :type 'removed :text "line2" :old-lineno 2 :new-lineno nil)
                (consult-jj-hunk-line-create
                 :type 'added :text "CHANGED2" :old-lineno nil :new-lineno 2)
                (consult-jj-hunk-line-create
                 :type 'context :text "line3" :old-lineno 3 :new-lineno 3))
   :old-start 1 :old-count 3 :new-start 1 :new-count 3
   :added 1 :removed 1 :supported t))

(defun consult-jj-hunk-test--binary ()
  "Return an unsupported `binary' hunk built by hand."
  (consult-jj-hunk-create
   :root "/r" :source-rev "@"
   :old-path "bin.dat" :new-path "bin.dat" :status 'binary
   :file-header "diff --git a/bin.dat b/bin.dat"
   :hunk-header nil :lines nil :supported nil))


(ert-deftest consult-jj-hunk-line->string-marks-each-type ()
  (should (equal (consult-jj-hunk-line->string
                  (consult-jj-hunk-line-create :type 'added :text "x"))
                 "+x"))
  (should (equal (consult-jj-hunk-line->string
                  (consult-jj-hunk-line-create :type 'removed :text "y"))
                 "-y"))
  (should (equal (consult-jj-hunk-line->string
                  (consult-jj-hunk-line-create :type 'context :text "z"))
                 " z")))

(ert-deftest consult-jj-hunk->patch-refuses-unsupported ()
  (should-error (consult-jj-hunk->patch (list (consult-jj-hunk-test--binary)))))

(ert-deftest consult-jj-hunk-export-diff-refuses-unsupported ()
  "Export errors before creating a buffer when a hunk is unsupported."
  (let ((before (buffer-list)))
    (should-error (consult-jj-hunk-export-diff
                   (list (consult-jj-hunk-test--binary))))
    ;; No stray export buffer was left behind.
    (should-not (cl-set-difference (buffer-list) before))))

(provide 'consult-jj-hunk-test)
;;; consult-jj-hunk-test.el ends here
