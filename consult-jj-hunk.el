;;; consult-jj-hunk.el --- Jujutsu hunk model for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A `consult-jj-hunk' represents one modified hunk from Jujutsu diff output.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff-mode)

(cl-defstruct (consult-jj-hunk
               (:constructor consult-jj-hunk-create)
               (:copier nil))
  "One Jujutsu diff hunk.

ROOT and SOURCE-REV identify the repository and revision the hunk came from.
OLD-PATH and NEW-PATH are the file's before/after paths.
STATUS is one of `modified', `added', `deleted', `renamed', `binary', or `mode'.

FILE-HEADER is the verbatim `diff --git' preamble; HUNK-HEADER the
verbatim `@@' line (nil for unsupported shapes).  LINES is a list of
`consult-jj-hunk-line'.  OLD-START/OLD-COUNT and NEW-START/NEW-COUNT are
the hunk ranges.  ADDED and REMOVED count changed lines.  SUPPORTED is
non-nil when the hunk carries textual changes that can be turned into a
patch."
  root source-rev
  old-path new-path status
  file-header hunk-header
  lines
  old-start old-count new-start new-count
  (added 0) (removed 0) supported)

(cl-defstruct (consult-jj-hunk-line
               (:constructor consult-jj-hunk-line-create)
               (:copier nil))
  "One line inside a hunk.
TYPE is `added', `removed', `context', or `no-newline'.  For content lines,
TEXT excludes the leading marker character; for `no-newline', it retains the
complete Git-format marker.  OLD-LINENO and NEW-LINENO are the line's number
on each side, nil where the line does not exist on that side."
  type text old-lineno new-lineno)

(defcustom consult-jj-hunk-diff-buffer-name "*consult-jj-diff*"
  "Name of the persistent buffer used to display modified-target diffs."
  :type 'string
  :group 'consult-jj)

;; TODO move to optional embark package
(defun consult-jj-hunk-export-diff (hunks)
  "Export HUNKS into a `diff-mode' buffer and return that buffer.
The patch is assembled with `consult-jj-hunk->patch', so an unsupported
hunk in HUNKS signals an error before any buffer is created or shown."
  (let ((patch (consult-jj-hunk->patch hunks))
        (buffer (get-buffer-create consult-jj-hunk-diff-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert patch))
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

(defun consult-jj-hunk->diff (hunks)
  "Assemble HUNKS into a unified diff string for display.
Consecutive hunks sharing a file header emit that header once.  Unsupported
hunks contribute their file-level metadata without signaling an error."
  (let ((out '())
        (last-header nil))
    (dolist (hunk hunks)
      (let ((file-header (consult-jj-hunk-file-header hunk)))
        (unless (equal file-header last-header)
          (push file-header out)
          (setq last-header file-header)))
      (when (consult-jj-hunk-supported hunk)
        (push (consult-jj-hunk-hunk-header hunk) out)
        (dolist (line (consult-jj-hunk-lines hunk))
          (push (consult-jj-hunk-line->string line) out))))
    (concat (string-join (nreverse out) "\n") "\n")))

(defun consult-jj-hunk->patch (hunks)
  "Assemble HUNKS into a unified diff patch string.
Consecutive hunks sharing a file header emit that header once.  Signal an
error if any hunk is not `supported'."
  (let ((out '())
        (last-header nil))
    (dolist (hunk hunks)
      (unless (consult-jj-hunk-supported hunk)
        (error "consult-jj: hunk for `%s' is not patchable (%s)"
               (consult-jj-hunk-preview-path hunk)
               (consult-jj-hunk-status hunk)))
      (let ((file-header (consult-jj-hunk-file-header hunk)))
        (unless (equal file-header last-header)
          (push file-header out)
          (setq last-header file-header)))
      (push (consult-jj-hunk-hunk-header hunk) out)
      (dolist (line (consult-jj-hunk-lines hunk))
        (push (consult-jj-hunk-line->string line) out)))
    (concat (string-join (nreverse out) "\n") "\n")))

(defun consult-jj-hunk-preview-path (hunk)
  "Return the path to preview for HUNK: its new path, else its old path."
  (or (consult-jj-hunk-new-path hunk) (consult-jj-hunk-old-path hunk)))

(defun consult-jj-hunk-first-changed-line (hunk)
  "Return the new-side line number of HUNK's first change.
Uses the first added line; for a pure deletion falls back to the hunk's
new-side start, and to 1 when unknown."
  (or (cl-loop for line in (consult-jj-hunk-lines hunk)
               when (eq (consult-jj-hunk-line-type line) 'added)
               return (consult-jj-hunk-line-new-lineno line))
      (consult-jj-hunk-new-start hunk)
      1))

(defun consult-jj-hunk-context (hunk)
  "Return the trailing context text of HUNK's `@@' header.
For an unsupported hunk with no header, return its status name."
  (let ((header (consult-jj-hunk-hunk-header hunk)))
    (if (and header (string-match "\\`@@.*@@\\(.*\\)\\'" header))
        (string-trim (match-string 1 header))
      (symbol-name (consult-jj-hunk-status hunk)))))

(defun consult-jj-hunk-line->string (line)
  "Return the unified-diff text for LINE, including its marker character."
  (pcase (consult-jj-hunk-line-type line)
    ('no-newline (consult-jj-hunk-line-text line))
    (type
     (concat (pcase type
               ('added "+")
               ('removed "-")
               (_ " "))
             (consult-jj-hunk-line-text line)))))

(provide 'consult-jj-hunk)
;;; consult-jj-hunk.el ends here
