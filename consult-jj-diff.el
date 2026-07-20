;;; consult-jj-diff.el --- Git-format diff parser for consult-jj -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A parser for Git-format diffs. This turns diff text into
;; `consult-jj-hunk' objects.

;;; Code:

;; TODO use rx for regexes

(require 'cl-lib)
(require 'subr-x)
(require 'consult-jj-hunk)

(defun consult-jj-diff-parse-diff (diff &optional root source-rev)
  "Parse git DIFF text into a list of `consult-jj-hunk'.
ROOT and SOURCE-REV are stamped onto every returned hunk when supplied."
  (mapcan (lambda (block)
            (consult-jj-diff--parse-file-block block root source-rev))
          (consult-jj-diff--split-files (split-string diff "\n"))))

(defun consult-jj-diff--split-files (lines)
  "Split diff LINES into per-file blocks, each starting at a `diff --git' line."
  (let ((blocks '())
        (current nil))
    (dolist (line lines)
      (if (string-prefix-p "diff --git " line)
          (progn
            (when current (push (nreverse current) blocks))
            (setq current (list line)))
        (when current (push line current))))
    (when current (push (nreverse current) blocks))
    (nreverse blocks)))

(defun consult-jj-diff--parse-file-block (block root source-rev)
  "Parse a file BLOCK into hunks stamped with ROOT and SOURCE-REV."
  (let ((old-path nil) (new-path nil) (status nil)
        (binary nil) (mode-change nil)
        (header '()) (body '()) (in-hunks nil))
    ;; Separate header lines (before the first @@) from hunk lines.
    (dolist (line block)
      (cond
       (in-hunks (push line body))
       ((string-prefix-p "@@" line) (setq in-hunks t) (push line body))
       (t (push line header))))
    (setq header (nreverse header)
          body (nreverse body))
    ;; Interpret the header to determine paths and status.
    (dolist (line header)
      (cond
       ((string-match "\\`diff --git \\([a-z]/.*\\) \\([a-z]/.*\\)\\'" line)
        (let ((a (match-string 1 line))
              (b (match-string 2 line)))
          (setq old-path (consult-jj-diff--strip-prefix a)
                new-path (consult-jj-diff--strip-prefix b))))
       ((string-prefix-p "new file mode" line) (setq status 'added))
       ((string-prefix-p "deleted file mode" line) (setq status 'deleted))
       ((string-prefix-p "rename from " line)
        (setq status 'renamed
              old-path (substring line (length "rename from "))))
       ((string-prefix-p "rename to " line)
        (setq status 'renamed
              new-path (substring line (length "rename to "))))
       ((or (string-prefix-p "old mode " line)
            (string-prefix-p "new mode " line))
        (setq mode-change t))
       ((string-prefix-p "Binary files " line) (setq binary t))
       ((string-match "\\`--- \\(.*\\)\\'" line)
        (let ((p (match-string 1 line)))
          (setq old-path (unless (string= p "/dev/null")
                           (consult-jj-diff--strip-prefix p)))))
       ((string-match "\\`\\+\\+\\+ \\(.*\\)\\'" line)
        (let ((p (match-string 1 line)))
          (setq new-path (unless (string= p "/dev/null")
                           (consult-jj-diff--strip-prefix p)))))))
    (setq status (cond (binary 'binary)
                       (status status)
                       ((and mode-change (null body)) 'mode)
                       (t 'modified)))
    (let ((file-header (string-join header "\n")))
      (if (null body)
          ;; No textual hunks: a single display-only, unsupported hunk.
          (list (consult-jj-hunk-create
                 :root root :source-rev source-rev
                 :old-path old-path :new-path new-path :status status
                 :file-header file-header :supported nil))
        (mapcar
         (lambda (h)
           (consult-jj-hunk-create
            :root root :source-rev source-rev
            :old-path old-path :new-path new-path :status status
            :file-header file-header :supported t
            :hunk-header (plist-get h :hunk-header)
            :old-start (plist-get h :old-start) :old-count (plist-get h :old-count)
            :new-start (plist-get h :new-start) :new-count (plist-get h :new-count)
            :lines (plist-get h :lines)
            :added (plist-get h :added) :removed (plist-get h :removed)))
         (consult-jj-diff--parse-hunk-blocks body))))))

(defconst consult-jj-diff--hunk-header-re
  "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regexp matching a unified-diff hunk header, capturing the four range numbers.")

(defun consult-jj-diff--parse-hunk-blocks (lines)
  "Parse hunk LINES (starting at an `@@' line) into a list of plists."
  (let ((hunks '())
        (cur nil)
        (old 0) (new 0))
    (cl-flet ((flush ()
                (when cur
                  (setf (plist-get cur :lines) (nreverse (plist-get cur :lines)))
                  (push cur hunks)
                  (setq cur nil))))
      (dolist (line lines)
        (cond
         ((string-match consult-jj-diff--hunk-header-re line)
          (flush)
          (setq old (string-to-number (match-string 1 line))
                new (string-to-number (match-string 3 line)))
          (setq cur (list :hunk-header line
                          :old-start old
                          :old-count (if (match-string 2 line)
                                         (string-to-number (match-string 2 line)) 1)
                          :new-start new
                          :new-count (if (match-string 4 line)
                                         (string-to-number (match-string 4 line)) 1)
                          :lines nil :added 0 :removed 0)))
         ((null cur) nil)
         ((string-prefix-p "\\" line) nil) ; "\ No newline at end of file"
         ((string-prefix-p "+" line)
          (push (consult-jj-hunk-line-create
                 :type 'added :text (substring line 1) :new-lineno new)
                (plist-get cur :lines))
          (cl-incf new)
          (cl-incf (plist-get cur :added)))
         ((string-prefix-p "-" line)
          (push (consult-jj-hunk-line-create
                 :type 'removed :text (substring line 1) :old-lineno old)
                (plist-get cur :lines))
          (cl-incf old)
          (cl-incf (plist-get cur :removed)))
         ((string-prefix-p " " line)
          (push (consult-jj-hunk-line-create
                 :type 'context :text (substring line 1)
                 :old-lineno old :new-lineno new)
                (plist-get cur :lines))
          (cl-incf old)
          (cl-incf new))))
      (flush))
    (nreverse hunks)))

;; TODO do I even need this?
(defun consult-jj-diff--strip-prefix (path)
  (if (save-match-data (string-match "\\`[a-z]/" path)) (substring path 2) path))

(provide 'consult-jj-diff)
;;; consult-jj-diff.el ends here
