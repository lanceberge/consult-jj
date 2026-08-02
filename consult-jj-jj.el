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
(require 'consult-jj-modified-file)

(defcustom consult-jj-jj-executable "jj"
  "Name of, or path to, the Jujutsu executable."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-jj-patch-executable "patch"
  "Name of, or path to, the patch executable used for hunk selection."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-show-git-bookmarks nil
  "Whether bookmark and tag discovery includes Git-backed revisions.
These references have the special remote name `git' and are displayed with an
`@git' suffix."
  :type 'boolean
  :group 'consult-jj)

(defconst consult-jj-jj--global-flags '("--no-pager" "--color" "never")
  "Switches passed to every jj invocation so output is plain and parseable.")

(defvar consult-jj-jj--squash-short-change-id nil
  "Jujutsu-supplied destination identity from the current squash operation.")

(defvar consult-jj-commit-two-line-mode nil
  "Non-nil when commit collection retains opaque two-row graph topology.")

(cl-defstruct (consult-jj-jj--modified-changes
               (:constructor consult-jj-jj--modified-changes-create)
               (:copier nil))
  "One captured modified-change snapshot."
  conflicted-paths file-statuses hunks)

(defun consult-jj-jj--commit-record-template (commit)
  "Return a JSON-record template for Jujutsu COMMIT expression."
  (concat
   "concat("
   "\"{\\\"change_id\\\":\", stringify(" commit
   ".change_id()).escape_json(),"
   "\",\\\"short_change_id\\\":\", stringify(" commit
   ".change_id().shortest(" commit
   ".change_id().short().len()).prefix()).escape_json(),"
   "\",\\\"change_id_unique\\\":\", stringify(" commit
   ".change_id().shortest(" commit
   ".change_id().short().len()).prefix()).escape_json(),"
   "\",\\\"change_id_remainder\\\":\", stringify(" commit
   ".change_id().shortest(" commit
   ".change_id().short().len()).rest()).escape_json(),"
   "\",\\\"commit_id\\\":\", stringify(" commit
   ".commit_id()).escape_json(),"
   "\",\\\"short_commit_id\\\":\", stringify(" commit
   ".commit_id().short()).escape_json(),"
   "\",\\\"change_offset\\\":\", json(" commit ".change_offset()),"
   "\",\\\"description\\\":\", " commit ".description().escape_json(),"
   "\",\\\"author_name\\\":\", " commit ".author().name().escape_json(),"
   "\",\\\"author_email\\\":\", stringify(" commit
   ".author().email()).escape_json(),"
   "\",\\\"timestamp\\\":\", " commit
   ".author().timestamp().format(\"%+\").escape_json(),"
   "\",\\\"bookmarks\\\":\", stringify(" commit
   ".local_bookmarks().map(|r| r.name()).join(\"\\0\")).escape_json(),"
   "\",\\\"remote_bookmarks\\\":\", stringify(" commit
   ".bookmarks().filter(|r| r.remote()).map("
   "|r| r.name() ++ \"@\" ++ r.remote()).join(\"\\0\")).escape_json(),"
   "\",\\\"tags\\\":\", stringify(" commit
   ".tags().map(|r| r.name() ++ if("
   "r.remote(), \"@\" ++ r.remote(), \"\")).join(\"\\0\")).escape_json(),"
   "\",\\\"working_copies\\\":\", stringify(" commit
   ".working_copies().map(|w| w.name()).join(\"\\0\")).escape_json(),"
   "\",\\\"divergent\\\":\", json(" commit ".divergent()),"
   "\",\\\"immutable\\\":\", json(" commit ".immutable()),"
   "\",\\\"empty\\\":\", json(" commit ".empty()),"
   "\",\\\"conflicted\\\":\", json(" commit ".conflict()),"
   "\",\\\"current\\\":\", json(" commit ".current_working_copy()),"
   "\",\\\"parent\\\":\", json(" commit
   ".contained_in(\"@-\")), \"}\")"))

(defconst consult-jj-jj--modified-changes-template
  (concat
   "concat("
   "\"{\\\"conflicts\\\":\","
   "json(self.conflicted_files().map(|entry| entry.path())),"
   "\",\\\"files\\\":[\","
   "self.diff().files().map(|entry| concat("
   "\"{\\\"path\\\":\", stringify(entry.path()).escape_json(),"
   "\",\\\"status\\\":\", entry.status().escape_json(), \"}\"))"
   ".join(\",\"), \"]}\","
   "\"\\0\", self.diff().git())")
  "Template returning Jujutsu file metadata and the Git-format diff.")

(defconst consult-jj-jj--bookmark-template
  (concat
   "concat("
   "\"{\\\"bookmark\\\":\", json(self),"
   "\",\\\"conflicted\\\":\", json(self.conflict()),"
   "\",\\\"normal_target\\\":\","
   "if(self.normal_target(), "
   (consult-jj-jj--commit-record-template "self.normal_target()")
   ","
   "\"null\"), \"}\\n\")")
  "Template used to serialize bookmark records and normal target commits.")

(defconst consult-jj-jj--log-template
  (concat (consult-jj-jj--commit-record-template "self") " ++ \"\\n\"")
  "Template used to serialize `jj log' commits as JSON lines.")

(defconst consult-jj-jj--two-line-log-template
  (concat
   "\"\\x1e\" ++ "
   (consult-jj-jj--commit-record-template "self")
   " ++ \"\\n\\x1f\\n\"")
  "Template separating opaque graph rows from commit records by sentinel.")

(defconst consult-jj-jj--log-graph-style-config
  "ui.graph.style=\"curved\""
  "Graph style enforced for parseable `jj log' topology.")

(defconst consult-jj-jj--log-node-config
  (concat
   "templates.log_node='"
   "coalesce(if(!self, \" \"), "
   "if(current_working_copy, \"@\"), "
   "if(immutable, \"◆\", \"○\"))'")
  "Canonical node symbols enforced for parseable `jj log' topology.")

(defconst consult-jj-jj--graph-connector-regexp
  "\\`[ \t│─├┤┬┴┼╭╮╯╰┌┐┘└]*\\'"
  "Regexp matching graph-only connector rows in the enforced style.")

(defconst consult-jj-jj--graph-direction-bits
  '((?╰ . 3) (?└ . 3) (?│ . 5) (?╭ . 6) (?┌ . 6) (?├ . 7)
    (?╯ . 9) (?┘ . 9) (?─ . 10) (?┴ . 11) (?╮ . 12) (?┐ . 12)
    (?┤ . 13) (?┬ . 14) (?┼ . 15))
  "Connection bits for glyphs in the enforced graph style.")

(defconst consult-jj-jj--graph-glyphs-by-direction
  '((3 . ?╰) (5 . ?│) (6 . ?╭) (7 . ?├)
    (9 . ?╯) (10 . ?─) (11 . ?┴) (12 . ?╮)
    (13 . ?┤) (14 . ?┬) (15 . ?┼))
  "Canonical compact glyph for each set of connection bits.")

(defun consult-jj-collect-commits (root revset)
  "Return structured Jujutsu log commits collected in ROOT for REVSET.
The `default' REVSET uses Jujutsu's configured `revsets.log'."
  (let ((output
         (apply #'consult-jj-jj--run
                root "log"
                (append
                 (when (stringp revset)
                   (list "--revision" revset))
                 (if consult-jj-commit-two-line-mode
                     (list "--template"
                           consult-jj-jj--two-line-log-template)
                   (list
                    "--config" consult-jj-jj--log-graph-style-config
                    "--config" consult-jj-jj--log-node-config
                    "--template" consult-jj-jj--log-template))))))
    (if consult-jj-commit-two-line-mode
        (consult-jj-jj--parse-two-line-graph-commits output)
      (consult-jj-jj--parse-graph-commits output))))

(defun consult-jj-collect-bookmarks (root)
  "Return visible structured local and remote Jujutsu bookmarks in ROOT."
  (let ((bookmarks
         (mapcar #'consult-jj-jj--parse-bookmark
                 (split-string
                  (consult-jj-jj--run
                   root "bookmark" "list" "--all-remotes"
                   "--quiet"
                   "--template" consult-jj-jj--bookmark-template)
                  "\n" t))))
    (if consult-jj-show-git-bookmarks
        bookmarks
      (cl-remove "git" bookmarks
                 :key #'consult-jj-bookmark-remote
                 :test #'equal))))

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

(defun consult-jj-collect-files (root &optional source-rev)
  "Return files modified by SOURCE-REV in ROOT, relative to ROOT.
SOURCE-REV defaults to the working-copy commit `@'."
  (split-string
   (apply
    #'consult-jj-jj--run root
    (append
     (when source-rev '("--ignore-working-copy"))
     (list "diff" "--name-only" "-r" (or source-rev "@"))))
   "\n" t))

(defun consult-jj-collect-modified-files (root &optional source-rev)
  "Return structured modified files for SOURCE-REV in ROOT.
SOURCE-REV defaults to the working-copy commit `@'.
Each object retains the authoritative conflict state and Git-format diff
snapshot obtained together in one Jujutsu invocation."
  (let ((changes
         (consult-jj-jj--collect-modified-changes root source-rev)))
    (consult-jj-jj--modified-files-from-hunks
     (consult-jj-jj--modified-changes-hunks changes)
     (consult-jj-jj--modified-changes-conflicted-paths changes)
     (consult-jj-jj--modified-changes-file-statuses changes))))

(defun consult-jj-collect-hunks (root &optional source-rev)
  "Return the modified hunks for SOURCE-REV in ROOT.
SOURCE-REV defaults to the working-copy commit `@'.
Conflict paths and the Git-format diff are captured by one Jujutsu invocation."
  (consult-jj-jj--modified-changes-hunks
   (consult-jj-jj--collect-modified-changes root source-rev)))

(defun consult-jj-jj--collect-modified-changes (root &optional source-rev)
  "Return one modified-change snapshot for SOURCE-REV under ROOT.
SOURCE-REV defaults to the working-copy commit `@'."
  (setq root (file-name-as-directory (expand-file-name root)))
  (let* ((source-rev (or source-rev "@"))
         (output
          (apply
           #'consult-jj-jj--run root
           (append
            (unless (equal source-rev "@")
              '("--ignore-working-copy"))
            (list
             "log" "--no-graph" "--revision" source-rev
             "--template" consult-jj-jj--modified-changes-template))))
         (separator (string-match "\0" output)))
    (unless separator
      (error "consult-jj: malformed modified-change snapshot"))
    (let* ((metadata
            (json-parse-string
             (substring output 0 separator)
             :object-type 'alist :array-type 'list
             :null-object nil :false-object nil))
           (conflicted-paths (alist-get 'conflicts metadata))
           (file-statuses (alist-get 'files metadata))
           (diff (substring output (1+ separator)))
           (hunks (consult-jj-diff-parse-diff diff root source-rev)))
      (dolist (hunk hunks)
        (setf
         (consult-jj-hunk-conflicted-p hunk)
         (and
          (member
           (or (consult-jj-hunk-new-path hunk)
               (consult-jj-hunk-old-path hunk))
          conflicted-paths)
          t)))
      (consult-jj-jj--modified-changes-create
       :conflicted-paths conflicted-paths
       :file-statuses file-statuses
       :hunks hunks))))

(defun consult-jj-jj--modified-files-from-hunks
    (hunks conflicted-paths file-statuses)
  "Build modified files from HUNKS, CONFLICTED-PATHS, and FILE-STATUSES."
  (let (groups)
    (dolist (hunk hunks)
      (let ((group (car (last groups))))
        (if (and group
                 (equal (consult-jj-hunk-file-header (car group))
                        (consult-jj-hunk-file-header hunk)))
            (setcdr group (append (cdr group) (list hunk)))
          (setq groups (append groups (list (list hunk)))))))
    (mapcar
     (lambda (group)
       (let* ((first (car group))
              (after-path (consult-jj-hunk-new-path first))
              (conflict-path (or after-path
                                 (consult-jj-hunk-old-path first)))
              (status-record
               (cl-find
                conflict-path file-statuses
                :key (lambda (record) (alist-get 'path record))
                :test #'equal))
              (status (alist-get 'status status-record)))
         (consult-jj-modified-file-create
          :source-rev (consult-jj-hunk-source-rev first)
          :before-path (consult-jj-hunk-old-path first)
          :after-path after-path
          :status (if status (intern status)
                    (consult-jj-hunk-status first))
          :hunks group
          :added (cl-loop for hunk in group
                          sum (consult-jj-hunk-added hunk))
          :removed (cl-loop for hunk in group
                            sum (consult-jj-hunk-removed hunk))
          :conflicted-p (and conflict-path
                             (member conflict-path conflicted-paths)))))
     groups)))

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

(defun consult-jj-jj--diff-files (files root &optional source-rev)
  "Return the Git-format diff for FILES in SOURCE-REV under ROOT.
SOURCE-REV defaults to the working-copy commit `@'."
  (let ((filesets (consult-jj-jj--exact-filesets files root)))
    (apply #'consult-jj-jj--run root
           (append
            (when source-rev '("--ignore-working-copy"))
            (list "diff" "--git" "-r" (or source-rev "@") "--")
            filesets))))

(defun consult-jj-jj--restore-hunks (hunks &optional root)
  "Restore HUNKS under ROOT with one Jujutsu restore operation."
  (consult-jj-jj--run-with-hunks
   hunks '("restore" "--changes-in" "@") 'forward root))

(defun consult-jj-jj--squash-hunks
    (hunks destination &optional root ignore-immutable source)
  "Squash HUNKS from SOURCE into DESTINATION under ROOT.
SOURCE defaults to the working-copy commit `@'.
When IGNORE-IMMUTABLE is non-nil, allow rewriting immutable commits.
Return `immutable' when confirmation is required, otherwise return the number
of conflicts introduced by the operation."
  (setq source (or source "@"))
  (consult-jj-jj--squash
   destination root ignore-immutable
   (lambda ()
     (consult-jj-jj--run-with-hunks
      hunks
      (append (when ignore-immutable '("--ignore-immutable"))
              (list "squash" "--from" source "--into" destination))
      'reverse root source))
   source))

(defun consult-jj-jj--squash-files
    (files destination root &optional ignore-immutable source)
  "Squash FILES from SOURCE into DESTINATION under ROOT.
SOURCE defaults to the working-copy commit `@'.
When IGNORE-IMMUTABLE is non-nil, allow rewriting immutable commits.
Return `immutable' when confirmation is required, otherwise return the number
of conflicts introduced by the operation."
  (setq source (or source "@"))
  (consult-jj-jj--squash
   destination root ignore-immutable
   (lambda ()
     (let ((filesets (consult-jj-jj--exact-filesets files root)))
       (apply #'consult-jj-jj--run root
              (append (when ignore-immutable '("--ignore-immutable"))
                      (list "squash" "--from" source "--into" destination "--")
                      filesets))))
   source))

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
          (destination-short-change-id
           (consult-jj-jj--revision-short-change-id destination root))
          (conflicts-before
           (consult-jj-jj--revision-conflicts destination root)))
      (setq consult-jj-jj--squash-short-change-id
            destination-short-change-id)
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

(defun consult-jj-jj--revision-short-change-id (revision root)
  "Return Jujutsu's shortest unique change ID for REVISION under ROOT."
  (string-trim
   (consult-jj-jj--run
    root "log" "--no-graph" "--revision" revision "--template"
    "change_id.shortest() ++ \"\\n\"")))

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
    (hunks command-args patch-direction &optional root source)
  "Run Jujutsu COMMAND-ARGS with HUNKS selected under ROOT.
PATCH-DIRECTION is `forward' when the command's editor starts at the parent
tree and `reverse' when it starts at the complete changed tree.  SOURCE
defaults to the working-copy commit `@'."
  (unless root
    (error "consult-jj: hunk operation requires repository context"))
  (unless (memq patch-direction '(forward reverse))
    (error "consult-jj: invalid patch direction `%s'" patch-direction))
  (let ((current-hunks (consult-jj-collect-hunks root source)))
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

(defun consult-jj-jj--parse-graph-commits (output)
  "Parse graph-prefixed commit records from Jujutsu OUTPUT."
  (let ((connectors (make-hash-table :test #'eq))
        (separators (make-hash-table :test #'eq))
        records)
    (dolist (line (split-string output "\n" t))
      (if (string-match "{" line)
          (push
           (cons (string-trim-right (substring line 0 (match-beginning 0)))
                 (substring line (match-beginning 0)))
           records)
        (when records
          (if (string-match-p
               consult-jj-jj--graph-connector-regexp line)
              (progn
                (puthash
                 (car records)
                 (cons (string-trim-right line)
                       (gethash (car records) connectors))
                 connectors)
                (setcar
                 (car records)
                 (consult-jj-jj--overlay-graph-row
                  (caar records) (string-trim-right line))))
            (puthash (car records) t separators)))))
    (setq records (nreverse records))
    (setq records
          (consult-jj-jj--connect-adjacent-graph-nodes
           records connectors separators))
    (let ((width
           (cl-loop for (graph . _) in records
                    maximize (string-width graph))))
      (cl-loop for (graph . record) in records
               collect
               (consult-jj-jj--parse-commit
                record
                (concat graph
                        (make-string (+ (- width (string-width graph)) 3)
                                     ?\s)))))))

(defun consult-jj-jj--parse-two-line-graph-commits (output)
  "Parse sentinel-separated commit records and opaque graph rows from OUTPUT."
  (let (commits current)
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "\x1e" line)
        (let ((sentinel-begin (match-beginning 0))
              (sentinel-end (match-end 0)))
          (setq current
                (consult-jj-jj--parse-commit
                 (substring line sentinel-end)))
          (setf (consult-jj-commit-two-line-graph-prefix current)
                (substring line 0 sentinel-begin)))
        (push current commits))
       ((and current (string-match "\x1f" line))
        (setf (consult-jj-commit-two-line-graph-continuation current)
              (substring line 0 (match-beginning 0))))))
    (nreverse commits)))

(defun consult-jj-jj--connect-adjacent-graph-nodes
    (records connectors separators)
  "Add topology tails to commit nodes in RECORDS.
CONNECTORS maps each record to graph-only rows folded into that record.
SEPARATORS identifies records followed by non-topology rows."
  (let* ((entries (vconcat records))
         (graphs (vconcat (mapcar #'car records)))
         (nodes
          (vconcat
           (mapcar
            (lambda (record)
              (consult-jj-jj--graph-node-column (car record)))
            records)))
         (bottom-connections (make-vector (length entries) nil)))
    (dotimes (index (length entries))
      (let* ((node-column (aref nodes index))
             (connector-rows
              (gethash (aref entries index) connectors)))
        (when node-column
          (aset
           bottom-connections index
           (cond
            (connector-rows
             (cl-some
              (lambda (connector)
                (consult-jj-jj--graph-glyph-connects-p
                 connector node-column 4))
              connector-rows))
            ((gethash (aref entries index) separators) nil)
            (t
             (and
              (< index (1- (length entries)))
              (let ((next-graph (aref graphs (1+ index))))
                (or
                 (equal node-column (aref nodes (1+ index)))
                 (consult-jj-jj--graph-glyph-connects-p
                  next-graph node-column 1))))))))))
    (cl-loop
     for (graph . record) in records
     for index from 0
     for node-column = (aref nodes index)
     for top-connected =
     (and
      node-column
      (> index 0)
      (if (equal node-column (aref nodes (1- index)))
          (aref bottom-connections (1- index))
        (consult-jj-jj--graph-glyph-connects-p
         (aref graphs (1- index)) node-column 4)))
     collect
     (cons
      (consult-jj-jj--decorate-graph-node
       graph node-column top-connected
       (aref bottom-connections index))
      record))))

(defun consult-jj-jj--graph-node-column (graph)
  "Return the character column of the commit node in GRAPH."
  (cl-position-if
   (lambda (glyph) (memq glyph '(?@ ?○ ?◆)))
   graph))

(defun consult-jj-jj--graph-glyph-connects-p (graph column direction)
  "Whether GRAPH's glyph at COLUMN connects in DIRECTION."
  (and
   (< column (length graph))
   (/= 0
       (logand
        direction
        (consult-jj-jj--graph-directions (aref graph column))))))

(defun consult-jj-jj--decorate-graph-node
    (graph node-column top-connected bottom-connected)
  "Render GRAPH's node with vertical connection tails at NODE-COLUMN."
  (if (not node-column)
      graph
    (concat
     (substring graph 0 node-column)
     (if (eq (aref graph node-column) ?@) "●"
       (substring graph node-column (1+ node-column)))
     (when top-connected "̍")
     (when bottom-connected "̩")
     (substring graph (1+ node-column)))))

(defun consult-jj-jj--overlay-graph-row (graph connector)
  "Overlay CONNECTOR topology onto the preceding commit GRAPH."
  (let ((width (max (length graph) (length connector))))
    (string-trim-right
     (apply
      #'string
      (cl-loop for index below width
               for graph-char = (or (and (< index (length graph))
                                         (aref graph index))
                                    ?\s)
               for connector-char =
               (or (and (< index (length connector))
                        (aref connector index))
                   ?\s)
               collect
               (consult-jj-jj--overlay-graph-char
                graph-char connector-char))))))

(defun consult-jj-jj--overlay-graph-char (graph-char connector-char)
  "Return the connected glyph for GRAPH-CHAR and CONNECTOR-CHAR."
  (cond
   ((memq graph-char '(?@ ?○ ?◆)) graph-char)
   ((eq graph-char ?\s) connector-char)
   ((eq connector-char ?\s) graph-char)
   ((and (eq graph-char ?│)
         (memq connector-char '(?╭ ?╮ ?╯ ?╰ ?┌ ?┐ ?┘ ?└)))
    connector-char)
   (t
    (or
     (alist-get
      (logior (consult-jj-jj--graph-directions graph-char)
              (consult-jj-jj--graph-directions connector-char))
      consult-jj-jj--graph-glyphs-by-direction)
     graph-char))))

(defun consult-jj-jj--graph-directions (glyph)
  "Return connection bits for curved graph GLYPH."
  (or
   (alist-get
    glyph
    consult-jj-jj--graph-direction-bits)
   0))

(defun consult-jj-jj--parse-commit (line &optional graph-prefix)
  "Parse one JSON log record from LINE with optional GRAPH-PREFIX."
  (let ((record (json-parse-string line :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)))
    (consult-jj-jj--commit-from-record record graph-prefix)))

(defun consult-jj-jj--commit-from-record (record &optional graph-prefix)
  "Build a structured commit from JSON alist RECORD with GRAPH-PREFIX.
RECORD may contain flat log fields or a serialized commit under `commit'."
  (let* ((commit (or (alist-get 'commit record) record))
         (author (alist-get 'author commit)))
    (consult-jj-commit-create
     :change-id (alist-get 'change_id commit)
     :short-change-id (alist-get 'short_change_id record)
     :change-id-unique (alist-get 'change_id_unique record)
     :change-id-remainder (alist-get 'change_id_remainder record)
     :commit-id (alist-get 'commit_id commit)
     :short-commit-id (alist-get 'short_commit_id record)
     :change-offset (alist-get 'change_offset record)
     :description (alist-get 'description commit)
     :author-name (or (alist-get 'author_name record)
                      (alist-get 'name author))
     :author-email (or (alist-get 'author_email record)
                       (alist-get 'email author))
     :timestamp (or (alist-get 'timestamp record)
                    (alist-get 'timestamp author))
     :bookmarks
     (split-string (or (alist-get 'bookmarks record) "") "\0" t)
     :remote-bookmarks
     (split-string (or (alist-get 'remote_bookmarks record) "") "\0" t)
     :tags (split-string (or (alist-get 'tags record) "") "\0" t)
     :working-copy-workspaces
     (split-string (or (alist-get 'working_copies record) "") "\0" t)
     :divergent-p (alist-get 'divergent record)
     :immutable-p (alist-get 'immutable record)
     :empty-p (alist-get 'empty record)
     :conflicted-p (alist-get 'conflicted record)
     :current-p (alist-get 'current record)
     :parent-p (alist-get 'parent record)
     :graph-prefix graph-prefix)))

(defun consult-jj-jj--parse-bookmark (line)
  "Parse one JSON bookmark record from LINE into a bookmark object."
  (let* ((serialized
          (json-parse-string line :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
         (record (alist-get 'bookmark serialized))
         (normal-target (alist-get 'normal_target serialized))
         (name (alist-get 'name record))
         (remote (alist-get 'remote record))
         (target (alist-get 'target record)))
    (consult-jj-bookmark-create
     :name name
     :remote remote
     :revision (if remote (concat name "@" remote) name)
     :target target
     :target-commit
     (and normal-target
          (consult-jj-jj--commit-from-record normal-target))
     :conflicted-p (alist-get 'conflicted serialized))))

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
