;;; consult-vc-hunk-test.el --- Tests for the provider-neutral hunk model -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `consult-vc-hunk' construction, the accessor helpers, and
;; patch/diff-buffer export, exercising the model directly rather than
;; through the Git parser.

;;; Code:

(require 'ert)
(require 'consult-vc-hunk)

(defun consult-vc-hunk-test--modified ()
  "Return a small supported `modified' hunk built by hand."
  (consult-vc-hunk-create
   :provider 'jj :root "/r" :source-rev "@"
   :old-path "mod.txt" :new-path "mod.txt" :status 'modified
   :file-header "diff --git a/mod.txt b/mod.txt
index b3c5a95..ef75112 100644
--- a/mod.txt
+++ b/mod.txt"
   :hunk-header "@@ -1,3 +1,3 @@ ctx"
   :lines (list (consult-vc-hunk-line-create
                 :type 'context :text "line1" :old-lineno 1 :new-lineno 1)
                (consult-vc-hunk-line-create
                 :type 'removed :text "line2" :old-lineno 2 :new-lineno nil)
                (consult-vc-hunk-line-create
                 :type 'added :text "CHANGED2" :old-lineno nil :new-lineno 2)
                (consult-vc-hunk-line-create
                 :type 'context :text "line3" :old-lineno 3 :new-lineno 3))
   :old-start 1 :old-count 3 :new-start 1 :new-count 3
   :added 1 :removed 1 :supported t))

(defun consult-vc-hunk-test--binary ()
  "Return an unsupported `binary' hunk built by hand."
  (consult-vc-hunk-create
   :provider 'jj :root "/r" :source-rev "@"
   :old-path "bin.dat" :new-path "bin.dat" :status 'binary
   :file-header "diff --git a/bin.dat b/bin.dat"
   :hunk-header nil :lines nil :supported nil))

(ert-deftest consult-vc-hunk-preview-path-prefers-new ()
  (should (equal (consult-vc-hunk-preview-path (consult-vc-hunk-test--modified))
                 "mod.txt"))
  (let ((deletion (consult-vc-hunk-create
                   :old-path "del.txt" :new-path nil :status 'deleted)))
    (should (equal (consult-vc-hunk-preview-path deletion) "del.txt"))))

(ert-deftest consult-vc-hunk-first-changed-line-uses-first-added ()
  (should (= (consult-vc-hunk-first-changed-line (consult-vc-hunk-test--modified))
             2)))

(ert-deftest consult-vc-hunk-first-changed-line-falls-back-to-new-start ()
  "A pure deletion has no added line, so fall back to the new-side start."
  (let ((deletion (consult-vc-hunk-create
                   :status 'deleted :new-start 7
                   :lines (list (consult-vc-hunk-line-create
                                 :type 'removed :text "gone" :old-lineno 7)))))
    (should (= (consult-vc-hunk-first-changed-line deletion) 7))))

(ert-deftest consult-vc-hunk-context-reads-header-tail ()
  (should (equal (consult-vc-hunk-context (consult-vc-hunk-test--modified)) "ctx")))

(ert-deftest consult-vc-hunk-context-falls-back-to-status ()
  "With no `@@' header, context reports the hunk status name."
  (should (equal (consult-vc-hunk-context (consult-vc-hunk-test--binary))
                 "binary")))

(ert-deftest consult-vc-hunk-line->string-marks-each-type ()
  (should (equal (consult-vc-hunk-line->string
                  (consult-vc-hunk-line-create :type 'added :text "x"))
                 "+x"))
  (should (equal (consult-vc-hunk-line->string
                  (consult-vc-hunk-line-create :type 'removed :text "y"))
                 "-y"))
  (should (equal (consult-vc-hunk-line->string
                  (consult-vc-hunk-line-create :type 'context :text "z"))
                 " z")))

(ert-deftest consult-vc-hunk->patch-emits-shared-header-once ()
  (let* ((h1 (consult-vc-hunk-test--modified))
         (h2 (consult-vc-hunk-test--modified))
         (patch (consult-vc-hunk->patch (list h1 h2))))
    (should (= (cl-count ?\n patch)
               ;; 4 file-header lines + (1 hunk-header + 4 body) * 2 hunks
               (+ 4 (* 2 5))))
    ;; The shared file header appears exactly once.
    (should (= (cl-loop with start = 0 with n = 0
                        while (string-match "diff --git a/mod.txt" patch start)
                        do (setq n (1+ n) start (match-end 0))
                        finally return n)
               1))
    (should (string-suffix-p "\n" patch))))

(ert-deftest consult-vc-hunk->patch-refuses-unsupported ()
  (should-error (consult-vc-hunk->patch (list (consult-vc-hunk-test--binary)))))

(ert-deftest consult-vc-hunk-export-diff-populates-diff-buffer ()
  (let ((buffer nil))
    (unwind-protect
        (progn
          (setq buffer (consult-vc-hunk-export-diff
                        (list (consult-vc-hunk-test--modified))))
          (should (bufferp buffer))
          (with-current-buffer buffer
            (should (derived-mode-p 'diff-mode))
            (should (string-match-p "^@@ -1,3 \\+1,3 @@" (buffer-string)))
            (should (string-match-p "^\\+CHANGED2$" (buffer-string)))
            (should (= (point) (point-min)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest consult-vc-hunk-export-diff-refuses-unsupported ()
  "Export errors before creating a buffer when a hunk is unsupported."
  (let ((before (buffer-list)))
    (should-error (consult-vc-hunk-export-diff
                   (list (consult-vc-hunk-test--binary))))
    ;; No stray export buffer was left behind.
    (should-not (cl-set-difference (buffer-list) before))))

(provide 'consult-vc-hunk-test)
;;; consult-vc-hunk-test.el ends here
