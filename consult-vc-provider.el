;;; consult-vc-provider.el --- Provider registry for consult-vc -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vc)

(defgroup consult-vc nil
  "Browse and act on version-control hunks with Consult and Embark."
  :group 'tools
  :prefix "consult-vc-")

(defcustom consult-vc-provider 'discover
  "The `consult-vc' backend provider to use.
When `discover' (the default), the provider is detected from the
repository's version-control backend: the backend symbol returned by
`vc-responsible-backend' is downcased and looked up in the registry, so
a jj repository selects the `jj' provider, a Git repository `git', and
so on.  Any other symbol forces that provider regardless of the backend."
  :type '(choice (const :tag "Discover from the VC backend" discover)
                 (const :tag "Jujutsu" jj)
                 (symbol :tag "Provider name"))
  :group 'consult-vc)

(cl-defstruct (consult-vc-provider-def
               (:constructor consult-vc-provider-def-create)
               (:copier nil))
  "Definition of a `consult-vc' backend provider.

NAME is the symbol used to look the provider up.  For `discover' to find
it, NAME must equal the downcased `vc-responsible-backend' symbol (e.g.
`jj', `git').

COLLECT-FILES and COLLECT-HUNKS are the REQUIRED data protocol; both are
validated at registration:

  COLLECT-FILES  (root) => list of modified/added/staged file paths.
  COLLECT-HUNKS  (root) => list of provider-neutral hunk objects.

ACTIONS is the OPTIONAL, provider-owned list of `consult-vc-action'
objects invocable on selected hunks."
  (name nil :read-only t)
  (collect-files nil)
  (collect-hunks nil)
  (actions nil))

(cl-defstruct (consult-vc-action
               (:constructor consult-vc-action-create)
               (:copier nil))
  "A thing that can be invoked on selected `consult-vc' hunks.
LABEL is the human-readable name shown in prompts and the Embark map.
KEY is its binding in the hunk keymap.  FN is called with the normalized
list of selected hunk objects."
  (label nil)
  (key nil)
  (fn nil))

(defvar consult-vc-provider--registry (make-hash-table :test 'eq)
  "Hash table mapping provider name symbols to `consult-vc-provider-def'.")

(defun consult-vc-provider-register (def)
  "Register provider DEF, replacing any provider with the same name.
Return DEF.  Signal an error if DEF is not a `consult-vc-provider-def' or
is missing a required protocol function."
  (cl-check-type def consult-vc-provider-def)
  (dolist (slot '(collect-files collect-hunks))
    (unless (functionp (cl-struct-slot-value 'consult-vc-provider-def slot def))
      (error "consult-vc provider `%s' is missing required function `%s'"
             (consult-vc-provider-def-name def) slot)))
  (puthash (consult-vc-provider-def-name def) def consult-vc-provider--registry)
  def)

(defun consult-vc-provider-get (name)
  "Return the registered `consult-vc-provider-def' for NAME, or nil."
  (gethash name consult-vc-provider--registry))

(defun consult-vc-provider-names ()
  "Return the list of registered provider name symbols."
  (hash-table-keys consult-vc-provider--registry))

(defun consult-vc-provider-resolve (root)
  "Return the `consult-vc-provider-def' to use for ROOT.
Honor `consult-vc-provider': `discover' detects ROOT's VC backend, any
other symbol forces that provider.  Signal a `user-error' when no
matching provider is registered."
  (let ((name (if (eq consult-vc-provider 'discover)
                  (consult-vc-provider--discover root)
                consult-vc-provider)))
    (or (consult-vc-provider-get name)
        (user-error "No consult-vc provider registered for `%s'" name))))

(defun consult-vc-provider--discover (root)
  "Return the provider name symbol for ROOT's version-control backend."
  (let ((backend (ignore-errors (vc-responsible-backend root))))
    (unless backend
      (user-error "consult-vc: %s is not under version control" root))
    (intern (downcase (symbol-name backend)))))

(provide 'consult-vc-provider)
;;; consult-vc-provider.el ends here
