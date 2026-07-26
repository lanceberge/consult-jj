;;; consult-jj-jj.el --- Jujutsu command execution for Consult JJ -*- lexical-binding: t; -*-

;; Author: Lance Bergeron
;; Keywords: vc, tools, convenience
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Internal utils for interfacing with the `jj' executable.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'consult-jj-bookmark)
(require 'consult-jj-commit)
(require 'consult-jj-diff)

(defcustom consult-jj-jj-executable "jj"
  "Name of, or path to, the Jujutsu executable."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-jj-patch-executable "patch"
  "Name of, or path to, the patch executable used for hunk selection."
  :type 'string
  :group 'consult-jj)

(defconst consult-jj-jj--global-flags '("--no-pager" "--color" "never")
  "Switches passed to every jj invocation so output is plain and parseable.")

;; TODO why did I let claude do this???
(defconst consult-jj-jj--log-template
  (concat
   "concat("
   "\"{\\\"change_id\\\":\", stringify(change_id).escape_json(),"
   "\",\\\"commit_id\\\":\", stringify(commit_id).escape_json(),"
   "\",\\\"description\\\":\", description.escape_json(),"
   "\",\\\"author_name\\\":\", author.name().escape_json(),"
   "\",\\\"author_email\\\":\", stringify(author.email()).escape_json(),"
   "\",\\\"timestamp\\\":\", author.timestamp().format(\"%+\").escape_json(),"
   "\",\\\"bookmarks\\\":\","
   "stringify(local_bookmarks.map(|b| b.name()).join(\"\\0\")).escape_json(),"
   "\",\\\"current\\\":\", current_working_copy,"
   "\",\\\"parent\\\":\", self.contained_in(\"@-\"), \"}\\n\")")
  "Template used to serialize `jj log' commits as JSON lines.")

(defun consult-jj-collect-commits (root revset)
  "Return structured Jujutsu log commits collected in ROOT for REVSET.
The `default' REVSET uses Jujutsu's configured `revsets.log'."
  (mapcar #'consult-jj-jj--parse-commit
          (split-string
           (apply #'consult-jj-jj--run
                  root "log" "--no-graph"
                  (append
                   (when (stringp revset)
                     (list "--revision" revset))
                   (list "--template" consult-jj-jj--log-template)))
           "\n" t)))

(defun consult-jj-collect-bookmarks (root)
  "Return every structured local and remote Jujutsu bookmark in ROOT."
  (mapcar #'consult-jj-jj--parse-bookmark
          (split-string
           (consult-jj-jj--run
            root "bookmark" "list" "--all-remotes"
            "--quiet"
            "--template"
            "json(self) ++ \"\\t\" ++ json(self.conflict()) ++ \"\\n\"")
           "\n" t)))

(defun consult-jj-jj--bookmark-set (name revision root)
  "Set local bookmark NAME at REVISION under ROOT."
  (consult-jj-jj--run root "bookmark" "set" name
                      "--revision" revision))

(defun consult-jj-jj--bookmark-move (name revision root)
  "Move local bookmark NAME to REVISION under ROOT."
  (consult-jj-jj--run root "bookmark" "move" (concat "exact:" name)
                      "--to" revision "--allow-backwards"))

(defun consult-jj-jj--bookmark-track (remote-bookmark root)
  "Track exact REMOTE-BOOKMARK under ROOT."
  (consult-jj-jj--run root "bookmark" "track" remote-bookmark))

(defun consult-jj-jj--bookmark-advance (names root)
  "Advance exact local bookmark NAMES under ROOT.
Return non-nil when Jujutsu reports an actual bookmark update."
  (let ((output
         (apply #'consult-jj-jj--run
                root "bookmark" "advance"
                (mapcar (lambda (name) (concat "exact:" name)) names))))
    (not (equal (string-trim output) "No bookmarks to update."))))

(defun consult-jj-jj--commit-parents (source root)
  "Return structured parent commits of SOURCE under ROOT."
  (mapcar
   #'consult-jj-jj--parse-commit
   (split-string
    (consult-jj-jj--run
     root "log" "--no-graph" "--revision" (concat source "-")
     "--template" consult-jj-jj--log-template)
    "\n" t)))

(defun consult-jj-jj--commit-description (revision root)
  "Return the complete description of REVISION under ROOT."
  (consult-jj-jj--run
   root "log" "--no-graph" "--revision" revision "--template" "description"))

(defun consult-jj-jj--resolve-single-revision (revision root)
  "Resolve REVISION to exactly one full commit ID under ROOT."
  (let ((commit-ids
         (split-string
          (consult-jj-jj--run
           root "log" "--no-graph" "--revision" revision
           "--template" "commit_id ++ \"\\n\"")
          "\n" t)))
    (unless (= (length commit-ids) 1)
      (user-error "consult-jj: Expected one commit, revision `%s' resolved to %d"
                  revision (length commit-ids)))
    (car commit-ids)))

(defun consult-jj-jj--abandon (revision root &optional ignore-immutable)
  "Abandon REVISION under ROOT.
When IGNORE-IMMUTABLE is non-nil, allow rewriting an immutable commit.
Return `immutable' when confirmation is required."
  (if (and (not ignore-immutable)
           (consult-jj-jj--revision-immutable-p revision root))
      'immutable
    (apply #'consult-jj-jj--run root
           (append
            (when ignore-immutable '("--ignore-immutable"))
            (list "abandon" revision)))))

(defun consult-jj-jj--describe (revision description root)
  "Replace REVISION's description with DESCRIPTION under ROOT."
  (consult-jj-jj--run root "describe" "--message" description revision))

(defun consult-jj-jj--duplicate
    (source root &optional destination placement)
  "Duplicate SOURCE relative to DESTINATION using PLACEMENT under ROOT."
  (let ((placement-flag
         (alist-get placement
                    '((onto . "--onto")
                      (after . "--insert-after")
                      (before . "--insert-before")))))
    (unless (memq placement '(nil onto after before))
      (error "consult-jj: invalid duplicate placement `%s'" placement))
    (when (and placement (null destination))
      (error "consult-jj: duplicate placement requires a destination"))
    (apply #'consult-jj-jj--run root
           (append
            (list "duplicate" source)
            (when placement-flag
              (list placement-flag destination))))))

(defun consult-jj-collect-files (root)
  "Return the list of modified files for `@' in ROOT, relative to ROOT."
  (split-string (consult-jj-jj--run root "diff" "--name-only" "-r" "@") "\n" t))

(defun consult-jj-collect-hunks (root)
  "Return the modified hunks for `@' in ROOT.
`jj diff --git' emits Git-format diffs, so parsing is delegated to
`consult-jj-diff-parse-diff'."
  (consult-jj-diff-parse-diff
   (consult-jj-jj--run root "diff" "--git" "-r" "@") root "@"))

(defun consult-jj-jj--new
    (anchor root &optional placement description no-edit)
  "Create a new empty commit relative to ANCHOR under ROOT.
PLACEMENT is `onto', `after', or `before'.  DESCRIPTION, when non-nil, is the
new commit description.  When NO-EDIT is non-nil, do not edit the new commit."
  (let ((placement-flag
         (alist-get placement
                    '((after . "--insert-after")
                      (before . "--insert-before")))))
    (unless (memq placement '(nil onto after before))
      (error "consult-jj: invalid new placement `%s'" placement))
    (apply #'consult-jj-jj--run root
           (append
            (list "new")
            (if placement-flag
                (list placement-flag anchor)
              (list anchor))
            (when description (list "--message" description))
            (when no-edit (list "--no-edit"))))))

(defun consult-jj-jj--rebase (source destination placement root selection)
  "Rebase SOURCE at DESTINATION using PLACEMENT and SELECTION under ROOT."
  (let ((placement-flag
         (alist-get placement
                    '((onto . "--onto")
                      (after . "--insert-after")
                      (before . "--insert-before"))))
        (selection-flag
         (alist-get selection
                    '((source . "--source")
                      (revision . "--revision")))))
    (unless placement-flag
      (error "consult-jj: invalid rebase placement `%s'" placement))
    (unless selection-flag
      (error "consult-jj: invalid rebase selection `%s'" selection))
    (consult-jj-jj--run root "rebase" selection-flag source
                        placement-flag destination)))

(defun consult-jj-jj--diff-files (files root)
  "Return the Git-format diff for FILES in `@' under ROOT."
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root
           (append '("diff" "--git" "-r" "@" "--") filesets))))

(defun consult-jj-jj--restore-hunks (hunks &optional root)
  "Restore HUNKS under ROOT with one Jujutsu restore operation."
  (consult-jj-jj--run-with-hunks
   hunks '("restore" "--changes-in" "@") 'forward root))

(defun consult-jj-jj--squash-hunks
    (hunks destination &optional root ignore-immutable)
  "Squash HUNKS from `@' into DESTINATION under ROOT.
When IGNORE-IMMUTABLE is non-nil, allow rewriting immutable commits.
Return `immutable' when confirmation is required, otherwise return the number
of conflicts introduced by the operation."
  (consult-jj-jj--squash
   destination root ignore-immutable
   (lambda ()
     (consult-jj-jj--run-with-hunks
      hunks
      (append (when ignore-immutable '("--ignore-immutable"))
              (list "squash" "--from" "@" "--into" destination))
      'reverse root))))

(defun consult-jj-jj--squash-files
    (files destination root &optional ignore-immutable)
  "Squash FILES from `@' into DESTINATION under ROOT.
When IGNORE-IMMUTABLE is non-nil, allow rewriting immutable commits.
Return `immutable' when confirmation is required, otherwise return the number
of conflicts introduced by the operation."
  (consult-jj-jj--squash
   destination root ignore-immutable
   (lambda ()
     (let ((filesets (consult-jj-jj--exact-filesets files root)))
       (apply #'consult-jj-jj--run root
              (append (when ignore-immutable '("--ignore-immutable"))
                      (list "squash" "--from" "@" "--into" destination "--")
                      filesets))))))

(defun consult-jj-jj--commit-squash
    (source destination description-policy root &optional ignore-immutable)
  "Squash the whole SOURCE commit into DESTINATION under ROOT.
DESCRIPTION-POLICY is `destination' or the exact description string to use.
When IGNORE-IMMUTABLE is non-nil, allow rewriting immutable commits.  Return
`immutable' when confirmation is required, otherwise return the number of
conflicts introduced by the operation."
  (consult-jj-jj--squash
   destination root ignore-immutable
   (lambda ()
     (apply
      #'consult-jj-jj--run root
      (append
       (when ignore-immutable '("--ignore-immutable"))
       (list "squash" "--from" source "--into" destination)
       (pcase description-policy
         ('destination '("--use-destination-message"))
         ((pred stringp) (list "--message" description-policy))
         (_ (error "consult-jj: invalid squash description policy `%s'"
                   description-policy))))))
   source))

(defun consult-jj-jj--squash
    (destination root ignore-immutable operation &optional source)
  "Run SOURCE squash OPERATION into DESTINATION under ROOT and report its result.
SOURCE defaults to the working-copy commit `@'.
IGNORE-IMMUTABLE permits immutable rewrites.  Return `immutable' instead of
running OPERATION when such permission is required.  Otherwise return the
number of conflict paths introduced in DESTINATION or its descendants."
  (setq source (or source "@"))
  (if (and (not ignore-immutable)
           (or (consult-jj-jj--revision-immutable-p source root)
               (consult-jj-jj--revision-immutable-p destination root)))
      'immutable
    (let ((destination-change-id
           (consult-jj-jj--revision-change-id destination root))
          (conflicts-before
           (consult-jj-jj--revision-conflicts destination root)))
      (funcall operation)
      (length
       (cl-set-difference
        (consult-jj-jj--revision-conflicts destination-change-id root)
        conflicts-before
        :test #'equal)))))

(defun consult-jj-jj--revision-change-id (revision root)
  "Return the full change ID for REVISION under ROOT."
  (string-trim
   (consult-jj-jj--run
    root "log" "--no-graph" "--revision" revision "--template"
    "change_id ++ \"\\n\"")))

(defun consult-jj-jj--revision-immutable-p (revision root)
  "Return non-nil when REVISION contains an immutable commit under ROOT."
  (string-match-p
   "^true$"
   (consult-jj-jj--run
    root "log" "--no-graph" "--revision" revision "--template"
    "if(self.immutable(), \"true\", \"false\") ++ \"\\n\"")))

(defun consult-jj-jj--revision-conflicts (revision root)
  "Return conflict identifiers in REVISION and its descendants under ROOT.
Each identifier combines a stable change ID and a conflicted path."
  (split-string
   (consult-jj-jj--run
    root "log" "--no-graph"
    "--revision" (format "(%s):: & conflicts()" revision)
    "--template"
    (concat
     "self.conflicted_files().map(|entry| "
     "change_id ++ \"\\0\" ++ entry.path() ++ \"\\n\").join(\"\")"))
   "\n" t))

(defun consult-jj-jj--split-hunks (hunks description root)
  "Split HUNKS from `@' with DESCRIPTION under ROOT."
  (consult-jj-jj--run-with-hunks
   hunks (list "split" "--revision" "@" "--message" description)
   'reverse root))

(defun consult-jj-jj--split-files (files description root)
  "Split FILES from `@' with DESCRIPTION under ROOT."
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root
           (append (list "split" "--revision" "@" "--message" description
                         "--")
                   filesets))))

(defun consult-jj-jj--run-with-hunks
    (hunks command-args patch-direction &optional root)
  "Run Jujutsu COMMAND-ARGS with HUNKS selected under ROOT.
PATCH-DIRECTION is `forward' when the command's editor starts at the parent
tree and `reverse' when it starts at the complete changed tree."
  (setq root (or root (consult-jj-hunk-root (car hunks))))
  (unless (memq patch-direction '(forward reverse))
    (error "consult-jj: invalid patch direction `%s'" patch-direction))
  (let ((current-hunks (consult-jj-collect-hunks root)))
    (unless (cl-every (lambda (hunk) (member hunk current-hunks)) hunks)
      (user-error "consult-jj: one or more selected hunks are stale"))
    (dolist (hunk hunks)
      (unless (consult-jj-hunk-supported hunk)
        (error "consult-jj: hunk for `%s' is not patchable (%s)"
               (consult-jj-hunk-preview-path hunk)
               (consult-jj-hunk-status hunk))))
    (let* ((paths (delete-dups
                   (mapcar #'consult-jj-hunk-preview-path hunks)))
           (path-hunks
            (cl-remove-if-not
             (lambda (hunk)
               (member (consult-jj-hunk-preview-path hunk) paths))
             current-hunks))
           (unselected
            (cl-remove-if (lambda (hunk) (member hunk hunks)) path-hunks))
           (filesets (consult-jj-jj--exact-filesets paths root)))
      (if (null unselected)
          (apply #'consult-jj-jj--run root
                 (append command-args '("--") filesets))
        (let* ((patch (consult-jj-hunk->patch unselected))
               (patch-file
                (make-temp-file "consult-jj-selection-" nil ".patch"))
               (edit-args
                (append
                 '("--force")
                 (when (eq patch-direction 'reverse) '("--reverse"))
                 (list "--strip=1" "--directory=$right"
                       (concat "--input=" patch-file)))))
          (unwind-protect
              (progn
                (with-temp-file patch-file
                  (insert patch))
                (apply
                 #'consult-jj-jj--run root
                 (append
                  (list
                   "--config"
                   (format "merge-tools.consult-jj-selection.program=%s"
                           (json-encode-string consult-jj-jj-patch-executable))
                   "--config"
                   (format "merge-tools.consult-jj-selection.edit-args=%s"
                           (json-encode edit-args)))
                  command-args
                  '("--interactive" "--tool" "consult-jj-selection" "--")
                  filesets)))
            (delete-file patch-file)))))))

(defun consult-jj-jj--restore-files (files &optional root)
  "Restore FILES under ROOT from the parent of the working-copy commit."
  (setq root (or root (locate-dominating-file (car files) ".jj")))
  (unless root
    (user-error "consult-jj: no Jujutsu repository found for `%s'" (car files)))
  (setq root (file-name-as-directory (expand-file-name root)))
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root "restore" "--" filesets)))

(defun consult-jj-jj--exact-filesets (files root)
  "Return exact Jujutsu filesets for FILES under ROOT."
  (mapcar
   (lambda (file)
     (let ((absolute (expand-file-name file root)))
       (unless (file-in-directory-p absolute root)
         (user-error "consult-jj: `%s' is outside `%s'" file root))
       (concat "root-file:"
               (json-encode-string (file-relative-name absolute root)))))
   files))

(defun consult-jj-jj--parse-commit (line)
  "Parse one JSON log record from LINE into a `consult-jj-commit'."
  (let ((record (json-parse-string line :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)))
    (consult-jj-commit-create
     :change-id (alist-get 'change_id record)
     :commit-id (alist-get 'commit_id record)
     :description (alist-get 'description record)
     :author-name (alist-get 'author_name record)
     :author-email (alist-get 'author_email record)
     :timestamp (alist-get 'timestamp record)
     :bookmarks (split-string (alist-get 'bookmarks record) "\0" t)
     :current-p (alist-get 'current record)
     :parent-p (alist-get 'parent record))))

(defun consult-jj-jj--parse-bookmark (line)
  "Parse one JSON bookmark record from LINE into a bookmark object."
  (pcase-let* ((`(,record-json ,conflicted-json)
                (split-string line "\t"))
               (record (json-parse-string record-json :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil))
               (conflicted-p (json-parse-string conflicted-json
                                                :false-object nil))
               (name (alist-get 'name record))
               (remote (alist-get 'remote record))
               (target (alist-get 'target record)))
    (consult-jj-bookmark-create
     :name name
     :remote remote
     :revision (if remote (concat name "@" remote) name)
     :target target
     :conflicted-p conflicted-p)))

(defun consult-jj-jj--run (root &rest args)
  "Run jj with ARGS in ROOT and return stdout as a string."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (apply #'process-file consult-jj-jj-executable nil t nil
                           (append consult-jj-jj--global-flags args))))
        (unless (and (integerp status) (zerop status))
          (user-error "consult-jj: jj failed: %s"
                      (string-trim (buffer-string))))
        (buffer-string)))))

(provide 'consult-jj-jj)
;;; consult-jj-jj.el ends here
