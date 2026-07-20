;;; consult-jj-diff-test.el --- Tests for the Git-format diff parser -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'ert)
(require 'consult-jj-diff)

(defconst consult-jj-diff-test--modified
  "diff --git a/mod.txt b/mod.txt
index b3c5a95..ef75112 100644
--- a/mod.txt
+++ b/mod.txt
@@ -1,5 +1,6 @@
 line1
-line2
+CHANGED2
 line3
 line4
-line5
+CHANGED5
+line6
")

(defconst consult-jj-diff-test--added
  "diff --git a/new.txt b/new.txt
new file mode 100644
index 0000000..1cb3c2c
--- /dev/null
+++ b/new.txt
@@ -0,0 +1,2 @@
+brand new
+content
")

(defconst consult-jj-diff-test--deleted
  "diff --git a/del.txt b/del.txt
deleted file mode 100644
index 988cd72..0000000
--- a/del.txt
+++ /dev/null
@@ -1,2 +0,0 @@
-gone1
-gone2
")

(defconst consult-jj-diff-test--binary
  "diff --git a/bin.dat b/bin.dat
index 0de32cd..ad0b33b 100644
Binary files a/bin.dat and b/bin.dat differ
")

(defconst consult-jj-diff-test--mode-only
  "diff --git a/modeonly.sh b/modeonly.sh
old mode 100644
new mode 100755
")

(defconst consult-jj-diff-test--rename-only
  "diff --git a/ren.txt b/renamed.txt
similarity index 100%
rename from ren.txt
rename to renamed.txt
")

(defconst consult-jj-diff-test--rename-changed
  "diff --git a/r.txt b/r2.txt
similarity index 75%
rename from r.txt
rename to r2.txt
index 71ac1b5..19638ca 100644
--- a/r.txt
+++ b/r2.txt
@@ -1,8 +1,8 @@
 a
-b
+B
 c
 d
 e
 f
 g
-h
+H
")

(ert-deftest consult-jj-diff-added-file ()
  (let ((h (car (consult-jj-diff-parse-diff consult-jj-diff-test--added))))
    (should (eq (consult-jj-hunk-status h) 'added))
    (should (null (consult-jj-hunk-old-path h)))
    (should (equal (consult-jj-hunk-new-path h) "new.txt"))
    (should (= (consult-jj-hunk-added h) 2))
    (should (= (consult-jj-hunk-removed h) 0))
    (should (consult-jj-hunk-supported h))))

(ert-deftest consult-jj-diff-deleted-file ()
  (let ((h (car (consult-jj-diff-parse-diff consult-jj-diff-test--deleted))))
    (should (eq (consult-jj-hunk-status h) 'deleted))
    (should (equal (consult-jj-hunk-old-path h) "del.txt"))
    (should (null (consult-jj-hunk-new-path h)))
    (should (= (consult-jj-hunk-removed h) 2))
    (should (consult-jj-hunk-supported h))))

(ert-deftest consult-jj-diff-binary-is-unsupported ()
  (let ((h (car (consult-jj-diff-parse-diff consult-jj-diff-test--binary))))
    (should (eq (consult-jj-hunk-status h) 'binary))
    (should-not (consult-jj-hunk-supported h))
    (should (null (consult-jj-hunk-lines h)))
    (should (null (consult-jj-hunk-hunk-header h)))))

(ert-deftest consult-jj-diff-rename-with-changes ()
  (let ((h (car (consult-jj-diff-parse-diff consult-jj-diff-test--rename-changed))))
    (should (eq (consult-jj-hunk-status h) 'renamed))
    (should (equal (consult-jj-hunk-old-path h) "r.txt"))
    (should (equal (consult-jj-hunk-new-path h) "r2.txt"))
    (should (consult-jj-hunk-supported h))
    (should (= (consult-jj-hunk-added h) 2))
    (should (= (consult-jj-hunk-removed h) 2))))

(ert-deftest consult-jj-diff-multiple-files-in-order ()
  (let* ((diff (concat consult-jj-diff-test--binary
                       consult-jj-diff-test--deleted
                       consult-jj-diff-test--modified
                       consult-jj-diff-test--added))
         (hunks (consult-jj-diff-parse-diff diff)))
    (should (= (length hunks) 4))
    (should (equal (mapcar #'consult-jj-hunk-status hunks)
                   '(binary deleted modified added)))))

(ert-deftest consult-jj-diff-line-typing-and-numbers ()
  (let* ((h (car (consult-jj-diff-parse-diff consult-jj-diff-test--modified)))
         (lines (consult-jj-hunk-lines h)))
    (should (equal (mapcar #'consult-jj-hunk-line-type lines)
                   '(context removed added context context removed added added)))
    ;; Context "line1" is old 1 / new 1; the first added "CHANGED2" is new 2.
    (should (= (consult-jj-hunk-line-old-lineno (nth 0 lines)) 1))
    (should (= (consult-jj-hunk-line-new-lineno (nth 0 lines)) 1))
    (should (= (consult-jj-hunk-line-new-lineno (nth 2 lines)) 2))
    (should (null (consult-jj-hunk-line-new-lineno (nth 1 lines))))))

(ert-deftest consult-jj-diff-patch-round-trip ()
  "Reassembling parsed hunks reproduces the original diff body."
  (let* ((hunks (consult-jj-diff-parse-diff consult-jj-diff-test--modified))
         (patch (consult-jj-hunk->patch hunks)))
    (should (string-match-p "^@@ -1,5 \\+1,6 @@" patch))
    (should (string-match-p "^\\+CHANGED2$" patch))
    (should (string-match-p "^-line5$" patch))
    (should (string-match-p "^ line3$" patch))))

(provide 'consult-jj-diff-test)
;;; consult-jj-diff-test.el ends here
