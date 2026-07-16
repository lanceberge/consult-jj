;;; consult-vc-hunk.el --- Provider-neutral hunk model for consult-vc -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; The provider-neutral hunk model.  A `consult-vc-hunk' is what every
;; backend's `collect-hunks' returns and what every action consumes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff-mode)

(cl-defstruct (consult-vc-hunk
               (:constructor consult-vc-hunk-create)
               (:copier nil))
  "One provider-neutral diff hunk.

PROVIDER, ROOT, and SOURCE-REV identify where the hunk came from.
OLD-PATH and NEW-PATH are the file's before/after paths.
STATUS is one of `modified', `added', `deleted', `renamed', `binary', or `mode'.

FILE-HEADER is the verbatim `diff --git' preamble; HUNK-HEADER the
verbatim `@@' line (nil for unsupported shapes).  LINES is a list of
`consult-vc-hunk-line'.  OLD-START/OLD-COUNT and NEW-START/NEW-COUNT are
the hunk ranges.  ADDED and REMOVED count changed lines.  SUPPORTED is
non-nil when the hunk carries textual changes that can be turned into a
patch."
  provider root source-rev
  old-path new-path status
  file-header hunk-header
  lines
  old-start old-count new-start new-count
  (added 0) (removed 0) supported)

(cl-defstruct (consult-vc-hunk-line
               (:constructor consult-vc-hunk-line-create)
               (:copier nil))
  "One line inside a hunk.
TYPE is `added', `removed', or `context'.  TEXT is the content without
the leading marker character.  OLD-LINENO and NEW-LINENO are the line's
number on each side, nil where the line does not exist on that side."
  type text old-lineno new-lineno)

(defun consult-vc-hunk-preview-path (hunk)
  "Return the path to preview for HUNK: its new path, else its old path."
  (or (consult-vc-hunk-new-path hunk) (consult-vc-hunk-old-path hunk)))

(defun consult-vc-hunk-first-changed-line (hunk)
  "Return the new-side line number of HUNK's first change.
Uses the first added line; for a pure deletion falls back to the hunk's
new-side start, and to 1 when unknown."
  (or (cl-loop for line in (consult-vc-hunk-lines hunk)
               when (eq (consult-vc-hunk-line-type line) 'added)
               return (consult-vc-hunk-line-new-lineno line))
      (consult-vc-hunk-new-start hunk)
      1))

(defun consult-vc-hunk-context (hunk)
  "Return the trailing context text of HUNK's `@@' header.
For an unsupported hunk with no header, return its status name."
  (let ((header (consult-vc-hunk-hunk-header hunk)))
    (if (and header (string-match "\\`@@.*@@\\(.*\\)\\'" header))
        (string-trim (match-string 1 header))
      (symbol-name (consult-vc-hunk-status hunk)))))

(defun consult-vc-hunk-line->string (line)
  "Return the unified-diff text for LINE, including its marker character."
  (concat (pcase (consult-vc-hunk-line-type line)
            ('added "+")
            ('removed "-")
            (_ " "))
          (consult-vc-hunk-line-text line)))

(defun consult-vc-hunk->patch (hunks)
  "Assemble HUNKS into a unified diff patch string.
Consecutive hunks sharing a file header emit that header once.  Signal an
error if any hunk is not `supported'."
  (let ((out '())
        (last-header nil))
    (dolist (hunk hunks)
      (unless (consult-vc-hunk-supported hunk)
        (error "consult-vc: hunk for `%s' is not patchable (%s)"
               (consult-vc-hunk-preview-path hunk)
               (consult-vc-hunk-status hunk)))
      (let ((file-header (consult-vc-hunk-file-header hunk)))
        (unless (equal file-header last-header)
          (push file-header out)
          (setq last-header file-header)))
      (push (consult-vc-hunk-hunk-header hunk) out)
      (dolist (line (consult-vc-hunk-lines hunk))
        (push (consult-vc-hunk-line->string line) out)))
    (concat (string-join (nreverse out) "\n") "\n")))

(defcustom consult-vc-hunk-diff-buffer-name "*consult-vc-diff*"
  "Name of the buffer populated by `consult-vc-hunk-export-diff'."
  :type 'string
  :group 'consult-vc)

(defun consult-vc-hunk-export-diff (hunks)
  "Export HUNKS into a `diff-mode' buffer and return that buffer.
The patch is assembled with `consult-vc-hunk->patch', so an unsupported
hunk in HUNKS signals an error before any buffer is created or shown."
  (let ((patch (consult-vc-hunk->patch hunks))
        (buffer (get-buffer-create consult-vc-hunk-diff-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert patch))
      (diff-mode)
      (goto-char (point-min)))
    (display-buffer buffer)
    buffer))

(provide 'consult-vc-hunk)
;;; consult-vc-hunk.el ends here
