;;; consult-vc-git-test.el --- Tests for the Git-format diff parser -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `consult-vc-git-parse-diff' across representative
;; Git-format diff shapes: modified (adjacent hunks), added, deleted,
;; renamed-with-changes, rename-only, binary, mode-only, multiple files,
;; plus a parse -> patch round trip.

;;; Code:

(require 'ert)
(require 'consult-vc-git)

(defconst consult-vc-git-test--modified
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

(defconst consult-vc-git-test--added
  "diff --git a/new.txt b/new.txt
new file mode 100644
index 0000000..1cb3c2c
--- /dev/null
+++ b/new.txt
@@ -0,0 +1,2 @@
+brand new
+content
")

(defconst consult-vc-git-test--deleted
  "diff --git a/del.txt b/del.txt
deleted file mode 100644
index 988cd72..0000000
--- a/del.txt
+++ /dev/null
@@ -1,2 +0,0 @@
-gone1
-gone2
")

(defconst consult-vc-git-test--binary
  "diff --git a/bin.dat b/bin.dat
index 0de32cd..ad0b33b 100644
Binary files a/bin.dat and b/bin.dat differ
")

(defconst consult-vc-git-test--mode-only
  "diff --git a/modeonly.sh b/modeonly.sh
old mode 100644
new mode 100755
")

(defconst consult-vc-git-test--rename-only
  "diff --git a/ren.txt b/renamed.txt
similarity index 100%
rename from ren.txt
rename to renamed.txt
")

(defconst consult-vc-git-test--rename-changed
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

(ert-deftest consult-vc-git-modified-single-hunk-adjacent-changes ()
  (let* ((hunks (consult-vc-git-parse-diff consult-vc-git-test--modified 'jj "/r" "@"))
         (h (car hunks)))
    (should (= (length hunks) 1))
    (should (eq (consult-vc-hunk-status h) 'modified))
    (should (equal (consult-vc-hunk-old-path h) "mod.txt"))
    (should (equal (consult-vc-hunk-new-path h) "mod.txt"))
    (should (eq (consult-vc-hunk-provider h) 'jj))
    (should (equal (consult-vc-hunk-source-rev h) "@"))
    (should (consult-vc-hunk-supported h))
    (should (= (consult-vc-hunk-old-start h) 1))
    (should (= (consult-vc-hunk-new-start h) 1))
    (should (= (consult-vc-hunk-added h) 3))
    (should (= (consult-vc-hunk-removed h) 2))
    ;; First change is the removal at new-side line 2, first *added* line is 2.
    (should (= (consult-vc-hunk-first-changed-line h) 2))))

(ert-deftest consult-vc-git-added-file ()
  (let ((h (car (consult-vc-git-parse-diff consult-vc-git-test--added))))
    (should (eq (consult-vc-hunk-status h) 'added))
    (should (null (consult-vc-hunk-old-path h)))
    (should (equal (consult-vc-hunk-new-path h) "new.txt"))
    (should (= (consult-vc-hunk-added h) 2))
    (should (= (consult-vc-hunk-removed h) 0))
    (should (consult-vc-hunk-supported h))))

(ert-deftest consult-vc-git-deleted-file ()
  (let ((h (car (consult-vc-git-parse-diff consult-vc-git-test--deleted))))
    (should (eq (consult-vc-hunk-status h) 'deleted))
    (should (equal (consult-vc-hunk-old-path h) "del.txt"))
    (should (null (consult-vc-hunk-new-path h)))
    (should (= (consult-vc-hunk-removed h) 2))
    (should (consult-vc-hunk-supported h))))

(ert-deftest consult-vc-git-binary-is-unsupported ()
  (let ((h (car (consult-vc-git-parse-diff consult-vc-git-test--binary))))
    (should (eq (consult-vc-hunk-status h) 'binary))
    (should-not (consult-vc-hunk-supported h))
    (should (null (consult-vc-hunk-lines h)))
    (should (null (consult-vc-hunk-hunk-header h)))))

(ert-deftest consult-vc-git-mode-only-is-unsupported ()
  (let ((h (car (consult-vc-git-parse-diff consult-vc-git-test--mode-only))))
    (should (eq (consult-vc-hunk-status h) 'mode))
    (should-not (consult-vc-hunk-supported h))))

(ert-deftest consult-vc-git-rename-only-is-unsupported ()
  (let ((h (car (consult-vc-git-parse-diff consult-vc-git-test--rename-only))))
    (should (eq (consult-vc-hunk-status h) 'renamed))
    (should (equal (consult-vc-hunk-old-path h) "ren.txt"))
    (should (equal (consult-vc-hunk-new-path h) "renamed.txt"))
    (should-not (consult-vc-hunk-supported h))))

(ert-deftest consult-vc-git-rename-with-changes ()
  (let ((h (car (consult-vc-git-parse-diff consult-vc-git-test--rename-changed))))
    (should (eq (consult-vc-hunk-status h) 'renamed))
    (should (equal (consult-vc-hunk-old-path h) "r.txt"))
    (should (equal (consult-vc-hunk-new-path h) "r2.txt"))
    (should (consult-vc-hunk-supported h))
    (should (= (consult-vc-hunk-added h) 2))
    (should (= (consult-vc-hunk-removed h) 2))))

(ert-deftest consult-vc-git-multiple-files-in-order ()
  (let* ((diff (concat consult-vc-git-test--binary
                       consult-vc-git-test--deleted
                       consult-vc-git-test--modified
                       consult-vc-git-test--added))
         (hunks (consult-vc-git-parse-diff diff)))
    (should (= (length hunks) 4))
    (should (equal (mapcar #'consult-vc-hunk-status hunks)
                   '(binary deleted modified added)))))

(ert-deftest consult-vc-git-line-typing-and-numbers ()
  (let* ((h (car (consult-vc-git-parse-diff consult-vc-git-test--modified)))
         (lines (consult-vc-hunk-lines h)))
    (should (equal (mapcar #'consult-vc-hunk-line-type lines)
                   '(context removed added context context removed added added)))
    ;; Context "line1" is old 1 / new 1; the first added "CHANGED2" is new 2.
    (should (= (consult-vc-hunk-line-old-lineno (nth 0 lines)) 1))
    (should (= (consult-vc-hunk-line-new-lineno (nth 0 lines)) 1))
    (should (= (consult-vc-hunk-line-new-lineno (nth 2 lines)) 2))
    (should (null (consult-vc-hunk-line-new-lineno (nth 1 lines))))))

(ert-deftest consult-vc-git-patch-round-trip ()
  "Reassembling parsed hunks reproduces the original diff body."
  (let* ((hunks (consult-vc-git-parse-diff consult-vc-git-test--modified))
         (patch (consult-vc-hunk->patch hunks)))
    (should (string-match-p "^@@ -1,5 \\+1,6 @@" patch))
    (should (string-match-p "^\\+CHANGED2$" patch))
    (should (string-match-p "^-line5$" patch))
    (should (string-match-p "^ line3$" patch))))

(ert-deftest consult-vc-git-patch-refuses-unsupported ()
  (let ((hunks (consult-vc-git-parse-diff consult-vc-git-test--binary)))
    (should-error (consult-vc-hunk->patch hunks))))

(provide 'consult-vc-git-test)
;;; consult-vc-git-test.el ends here
