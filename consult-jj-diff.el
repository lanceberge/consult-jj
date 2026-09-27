;;; consult-jj-diff.el --- Revision comparison browser and Git diff parser -*- lexical-binding: t; -*-

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
(require 'json)
(require 'consult)
(require 'consult-jj-hunk)
(require 'consult-jj-modified-file)

(defvar consult-jj--commit-modified-root)
(defvar consult-jj--diff-preview-buffer-name)
(defvar consult-jj-jj-patch-executable)
(declare-function consult-jj--root "consult-jj")
(declare-function consult-jj-read-revision "consult-jj-revision")
(declare-function consult-jj--revision-id "consult-jj-revision")
(declare-function consult-jj--live-candidate-collection "consult-jj-session")
(declare-function consult-jj--diff-preview-state "consult-jj")
(declare-function consult-jj--modified-target-kind "consult-jj")
(declare-function consult-jj--display-diff "consult-jj")
(declare-function consult-jj-jj--run "consult-jj-jj")
(declare-function consult-jj-jj--modified-files-from-hunks "consult-jj-jj")

(defvar consult-jj-diff--from nil "FROM revision of the active Diff browser.")
(defvar consult-jj-diff--to nil "TO revision of the active Diff browser.")
(defvar consult-jj-diff--root nil "Root of the active Diff browser.")

;;;###autoload
(defun consult-jj-diff (&optional from to)
  "Browse the directional tree diff from FROM to TO.
Read either missing revision through `consult-jj-read-revision'.  Bookmark
and Tag references stay symbolic; Commit objects use exact commit IDs."
  (interactive)
  (let* ((root (file-name-as-directory (expand-file-name (consult-jj--root))))
         (default-directory root))
    (setq from (or from (consult-jj-read-revision "Diff from: " root)))
    (when from
      (setq to (or to (consult-jj-read-revision "Diff to: " root)))
      (when to
        (let ((consult-jj-diff--from (consult-jj--revision-id from))
              (consult-jj-diff--to (consult-jj--revision-id to))
              (consult-jj-diff--root root))
          (consult-jj-diff--browse-files))))))

(defun consult-jj-diff--browse-files ()
  "Browse changed files for the current directional comparison."
  (let* ((files (consult-jj-diff--collect-files
                 consult-jj-diff--root nil
                 (cons consult-jj-diff--from consult-jj-diff--to)))
         (context (list consult-jj-diff--from consult-jj-diff--to
                        consult-jj-diff--root))
         (candidates (consult-jj-diff--file-candidates files context)))
    (when-let* ((file
                (consult--read
                 (consult-jj--live-candidate-collection
                  candidates consult-jj-diff--root 'diff-file nil
                  #'consult-jj-diff--collect-files
                  (lambda (refreshed _root)
                    (consult-jj-diff--file-candidates refreshed context))
                  (cons consult-jj-diff--from consult-jj-diff--to))
                 :prompt "Diff files: "
                 :category 'consult-jj-diff-file
                 :require-match t :sort nil
                 :lookup #'consult-jj-diff--lookup-file
                 :state (consult-jj--diff-preview-state
                         (lambda (target)
                           (consult-jj-hunk->diff
                            (consult-jj-modified-file-hunks target)))
                         consult-jj--diff-preview-buffer-name))))
      (consult-jj-diff-show (list file)))))

(defun consult-jj-diff--collect-files (root _tier revisions)
  "Collect changed files under ROOT for ordered REVISIONS."
  (let* ((diff (consult-jj-jj--run
                root "--ignore-working-copy" "diff" "--git"
                "--from" (car revisions) "--to" (cdr revisions)))
         (hunks (consult-jj-diff-parse-diff diff root)))
    (consult-jj-jj--modified-files-from-hunks hunks nil nil)))

(defun consult-jj-diff--file-candidates (files &optional context)
  "Present comparison FILES in source order with CONTEXT."
  (cl-loop for file in files
           for index from 0
           collect (propertize
                    (or (consult-jj-modified-file-after-path file)
                        (consult-jj-modified-file-before-path file))
                    'consult-jj-diff-file file
                    'consult-jj-diff-context context
                    'consult-jj-diff-order index)))

(defun consult-jj-diff--lookup-file (selected candidates &rest _)
  "Return SELECTED's file from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-diff-file candidate)))

;;;###autoload
(defun consult-jj-diff-hunks (files)
  "Browse one collective hunk list from comparison FILES."
  (interactive)
  (unless (and (proper-list-p files) files
               (cl-every #'consult-jj-modified-file-p files))
    (user-error "consult-jj: Diff hunks require a file target set"))
  (let* ((paths (mapcar (lambda (file)
                          (or (consult-jj-modified-file-after-path file)
                              (consult-jj-modified-file-before-path file)))
                        files))
         (hunks (cl-mapcan (lambda (file)
                            (copy-sequence
                             (consult-jj-modified-file-hunks file)))
                          files))
         (context (list consult-jj-diff--from consult-jj-diff--to
                        consult-jj-diff--root))
         (candidates (consult-jj-diff--hunk-candidates hunks context)))
    (when-let* ((hunk
                (consult--read
                 (if (and consult-jj-diff--root
                          consult-jj-diff--from consult-jj-diff--to)
                     (consult-jj--live-candidate-collection
                      candidates consult-jj-diff--root 'diff-hunk nil
                      #'consult-jj-diff--collect-files
                      (lambda (refreshed _root)
                        (consult-jj-diff--hunk-candidates
                         (cl-mapcan
                          (lambda (file)
                            (when (member
                                   (or (consult-jj-modified-file-after-path file)
                                       (consult-jj-modified-file-before-path file))
                                   paths)
                              (copy-sequence
                               (consult-jj-modified-file-hunks file))))
                          refreshed)
                         context))
                      (cons consult-jj-diff--from consult-jj-diff--to))
                   candidates)
                 :prompt "Diff hunks: "
                 :category 'consult-jj-diff-hunk
                 :require-match t :sort nil
                 :lookup #'consult-jj-diff--lookup-hunk
                 :state (consult-jj--diff-preview-state
                         (lambda (target)
                           (consult-jj-hunk->diff (list target)))
                         consult-jj--diff-preview-buffer-name))))
      (consult-jj-diff-show (list hunk)))))

(defun consult-jj-diff--hunk-candidates (hunks &optional context)
  "Present HUNKS in source order with Diff action identity and CONTEXT."
  (cl-loop for hunk in hunks
           for index from 0
           collect (propertize
                    (consult--tofu-append
                     (format "%s:%s %s"
                             (consult-jj-hunk-preview-path hunk)
                             (or (consult-jj-hunk-new-start hunk) 1)
                             (consult-jj-hunk-context hunk))
                     index)
                    'consult-jj-diff-hunk hunk
                    'consult-jj-diff-context context
                    'consult-jj-diff-order index)))

(defun consult-jj-diff--lookup-hunk (selected candidates &rest _)
  "Return SELECTED's hunk from CANDIDATES."
  (when-let* ((candidate (car (member selected candidates))))
    (get-text-property 0 'consult-jj-diff-hunk candidate)))

;;;###autoload
(defun consult-jj-diff-show (targets &optional root)
  "Display comparison TARGETS in the reusable diff buffer under ROOT."
  (interactive)
  (unless (and (proper-list-p targets) targets)
    (user-error "consult-jj: Diff display needs a target set"))
  (let* ((kind (consult-jj--modified-target-kind targets "Diff display"))
         (hunks (if (eq kind 'file)
                    (cl-mapcan (lambda (file)
                                 (copy-sequence
                                  (consult-jj-modified-file-hunks file)))
                               targets)
                  targets)))
    (consult-jj--display-diff
     (consult-jj-hunk->diff hunks)
     consult-jj-hunk-diff-buffer-name
     (or root consult-jj-diff--root (consult-jj--root)))))

;;;###autoload
(defun consult-jj-diff-revert (targets &optional from to root)
  "Undo comparison TARGETS from FROM to TO in the working-copy commit.
TARGETS must be a homogeneous list of comparison files or hunks.  The
selected inverse patch is applied to `@' in one Jujutsu diffedit operation.
FROM and TO default to the active Diff browser's ordered revisions."
  (interactive)
  (unless (and (proper-list-p targets) targets)
    (user-error "consult-jj: Diff revert needs a target set"))
  (let* ((kind (consult-jj--modified-target-kind targets "Diff revert"))
         (hunks (if (eq kind 'file)
                    (cl-mapcan (lambda (file)
                                 (copy-sequence
                                  (consult-jj-modified-file-hunks file)))
                               targets)
                  targets))
         (patch (consult-jj-hunk->patch hunks))
         (root (file-name-as-directory
                (expand-file-name
                 (or root consult-jj-diff--root (consult-jj--root)))))
         (from (or from consult-jj-diff--from))
         (to (or to consult-jj-diff--to)))
    (unless (and from to)
      (user-error "consult-jj: Diff revert needs FROM and TO revisions"))
    (let ((patch-file (make-temp-file "consult-jj-diff-revert-" nil ".patch"))
          (consult-jj--commit-modified-root root))
      (unwind-protect
          (progn
            (with-temp-file patch-file (insert patch))
            (consult-jj-jj--run
             root
             "--config"
             (format "merge-tools.consult-jj-diff-revert.program=%s"
                     (json-encode-string consult-jj-jj-patch-executable))
             "--config"
             (format "merge-tools.consult-jj-diff-revert.edit-args=%s"
                     (json-encode
                      (list "--batch" "--fuzz=0" "--reverse"
                            "--strip=1" "--directory=$right"
                            (concat "--input=" patch-file))))
             "diffedit" "--revision" "@"
             "--tool" "consult-jj-diff-revert")
            (run-hooks 'consult-jj-commit-modified-hook))
        (delete-file patch-file)))))

(defun consult-jj-diff-parse-diff (diff &optional root source-rev)
  "Parse git DIFF text into a list of `consult-jj-hunk'.
SOURCE-REV is stamped onto every returned hunk and defaults to `@'.
ROOT is accepted as collection context but is not retained by targets."
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
  "Parse a file BLOCK into hunks stamped with SOURCE-REV.
ROOT is collection context and is not retained by the returned targets."
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
                 :source-rev source-rev
                 :old-path old-path :new-path new-path :status status
                 :file-header file-header :supported nil))
        (mapcar
         (lambda (h)
           (consult-jj-hunk-create
            :source-rev source-rev
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
         ((string-prefix-p "\\" line)
          (push (consult-jj-hunk-line-create
                 :type 'no-newline :text line)
                (plist-get cur :lines)))
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
