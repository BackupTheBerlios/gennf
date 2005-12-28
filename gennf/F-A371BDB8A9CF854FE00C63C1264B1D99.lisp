;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;; Clear out requires for mcvs-upgrade to work right.
;(setf *modules* nil)

;(require "create")
;(require "checkout")
;(require "grab")
;(require "add")
;(require "remove")
;(require "move")
;(require "link")
;(require "update")
;(require "filt")
;(require "generic")
;(require "convert")
;(require "branch")
;(require "remap")
;(require "purge")
;(require "restore")
;(require "prop")
;(require "watch")
;(require "split")
;(require "restart")
;(require "error")
;(require "options")
;(require "find-bind")
;(provide "mcvs-main")

(define-option-constant *global-options* 
  (0 arg "H" "help" "Q" "q" "r" "w" "l" "n" "t" "v" "f" "version"
	 "meta" "metaonly" "nometa" "error-continue" "error-terminate" "debug")
  (1 arg "T" "e" "d" "r" "z" "s" "i" "up"))

(define-option-constant *help-options*)

(define-option-constant *create-options* 
  (0 arg "d")
  (1 arg "k" "I" "b" "m" "W"))

(define-option-constant *grab-options* 
  (0 arg "A") 
  (1 arg "r"))

(define-option-constant *checkout-options* 
  (0 arg "f") 
  (1 arg "r" "D" "d" "k" "j"))

(define-option-constant *export-options* 
  (0 arg "f") 
  (1 arg "r" "D" "d" "k"))

(define-option-constant *add-options* 
  (0 arg "R") 
  (1 arg "k" "m"))

(define-option-constant *remove-options* 
  (0 arg "R"))

(define-option-constant *update-options* 
  (0 arg "A" "C" "f" "p") 
  (1 arg "k" "r" "D" "j" "I" "W"))

(define-option-constant *switch-options* 
  (1 arg "k" "I" "W"))

(define-option-constant *commit-options* 
  (0 arg "f") 
  (1 arg "F" "m" "r"))

(define-option-constant *diff-options* 
  (0 arg "a" "b" "B" "brief" "c" "d" "e" "ed" "expand-tabs" "f" "forward-ed"
	 "H" "i" "ignore-all-space" "ignore-blank-lines" "ignore-case"
	 "ignore-space-change" "initial-tab" "l" "left-column" "minimal" "n"
	 "N" "new-file" "p" "P" "--paginate" "q" "rcs" "report-identical-files"
	 "s" "show-c-function" "side-by-side" "speed-large-files"
	 "suppress-common-lines" "t" "T" "text" "u" "unidirectional-new-file"
	 "w" "y") 
  (1 arg "C" "context" "D" "F" "horizon-lines" "ifdef" "ignore-matching-lines"
	 "L" "label" "line-format" "new-group-format" "new-line-format"
	 "old-group-format" "old-line-format" "r" "show-function-line"
	 "unchanged-group-format" "unchanged-line-format" "U" "unified" "W"
	 "width"))

(define-option-constant *tag-options* 
  (0 arg "l" "d" "f" "b" "F" "c") 
  (1 arg "r" "D"))

(define-option-constant *log-options* 
  (0 arg "R" "h" "t" "N" "b")
  (1 arg "r" "d" "s" "w"))

(define-option-constant *status-options* 
  (0 arg "v"))

(define-option-constant *annotate-options*
  (0 arg "f")
  (1 arg "r" "D"))

(define-option-constant *filt-options* 
  (1 arg "r" "D"))

(define-option-constant *remote-filt-options* 
  (1 arg "r" "D"))

(define-option-constant *move-options*)
(define-option-constant *link-options*)
(define-option-constant *convert-options*)
(define-option-constant *branch-options*)

(define-option-constant *merge-options*
  (1 arg "k"))

(define-option-constant *remerge-options*
  (1 arg "k"))

(define-option-constant *list-branches-options*)
(define-option-constant *remap-options*)
(define-option-constant *purge-options*)
(define-option-constant *restore-options*)

(define-option-constant *prop-options*
  (1 arg "set" "clear" "remove")
  (2 arg "value"))

(define-option-constant *watch-options*
  (0 arg "on" "off")
  (1 arg "add" "remove"))

(define-option-constant *watchers-options*)
(define-option-constant *edit-options*)
(define-option-constant *unedit-options*)
(define-option-constant *editors-options*)
(define-option-constant *sync-to-cvs-options*)
(define-option-constant *sync-from-cvs-options*)

(declaim (special *usage* *mcvs-command-table*))

(defparameter *mcvs-command-table*
 `(
;("help" ,#'mcvs-help nil ,*help-options*)
   ("create" ,#'mcvs-create-wrapper ,*create-help* ,*create-options*)
   ("grab" ,#'mcvs-grab-wrapper ,*grab-help* ,*grab-options*)
   ("checkout" ,#'mcvs-checkout-wrapper ,*checkout-help* ,*checkout-options*)
   ("co" ,#'mcvs-checkout-wrapper ,*checkout-help* ,*checkout-options*)
   ("export" ,#'mcvs-export-wrapper ,*export-help* ,*export-options*)
   ("ex" ,#'mcvs-export-wrapper ,*export-help* ,*export-options*)
   ("add" ,#'mcvs-add-wrapper ,*add-help* ,*add-options*)
   ("remove" ,#'mcvs-remove-wrapper ,*remove-help* ,*remove-options*)
   ("rm" ,#'mcvs-remove-wrapper ,*remove-help* ,*remove-options*)
   ("move" ,#'mcvs-move-wrapper ,*move-help* ,*move-options*)
   ("mv" ,#'mcvs-move-wrapper ,*move-help* ,*move-options*)
   ("link" ,#'mcvs-link-wrapper ,*link-help* ,*link-options*)
   ("ln" ,#'mcvs-link-wrapper ,*link-help* ,*link-options*)
   ("update" ,#'mcvs-update-wrapper nil ,*update-options*)
   ("up" ,#'mcvs-update-wrapper nil ,*update-options*)
   ("commit" ,#'mcvs-commit-wrapper nil ,*commit-options*)
   ("ci" ,#'mcvs-commit-wrapper nil ,*commit-options*)
   ("diff" ,#'mcvs-diff-wrapper nil ,*diff-options*)
   ("tag" ,#'mcvs-tag-wrapper nil ,*tag-options*)
   ("log" ,#'mcvs-log-wrapper nil ,*log-options*)
   ("status" ,#'mcvs-status-wrapper nil ,*status-options*)
   ("stat" ,#'mcvs-status-wrapper nil ,*status-options*)
   ("annotate" ,#'mcvs-annotate-wrapper nil ,*annotate-options*)
   ("filt" ,#'mcvs-filt-wrapper nil ,*filt-options*)
   ("fi" ,#'mcvs-filt-wrapper nil ,*filt-options*)
   ("remote-filt" ,#'mcvs-remote-filt-wrapper nil ,*remote-filt-options*)
   ("rfilt" ,#'mcvs-remote-filt-wrapper nil ,*remote-filt-options*)
   ("rfi" ,#'mcvs-remote-filt-wrapper nil ,*remote-filt-options*)
   ("convert" ,#'mcvs-convert-wrapper ,*convert-help* ,*convert-options*)
   ("branch" ,#'mcvs-branch-wrapper ,*branch-help* ,*branch-options*)
   ("switch" ,#'mcvs-switch-wrapper nil ,*switch-options*)
   ("sw" ,#'mcvs-switch-wrapper nil ,*switch-options*)
   ("merge" ,#'mcvs-merge-wrapper nil ,*merge-options*)
   ("remerge" ,#'mcvs-remerge-wrapper nil ,*remerge-options*)
   ("list-branches" ,#'mcvs-list-branches-wrapper nil ,*list-branches-options*)
   ("lb" ,#'mcvs-list-branches-wrapper nil ,*list-branches-options*)
   ("purge" ,#'mcvs-purge-wrapper nil ,*purge-options*)
   ("restore" ,#'mcvs-restore-wrapper nil ,*restore-options*)
   ("remap" ,#'mcvs-remap-wrapper nil ,*remap-options*)
   ("prop" ,#'mcvs-prop-wrapper nil ,*prop-options*)
   ("watch" ,#'mcvs-watch-wrapper nil ,*watch-options*)
   ("watchers" ,#'mcvs-watchers-wrapper nil ,*watchers-options*)
   ("edit" ,#'mcvs-edit-wrapper nil ,*edit-options*)
   ("unedit" ,#'mcvs-unedit-wrapper nil ,*unedit-options*)
   ("editors" ,#'mcvs-editors-wrapper nil ,*editors-options*)
   ("sync-from-cvs" ,#'mcvs-sync-from-wrapper nil ,*editors-options*)
   ("sync-to-cvs" ,#'mcvs-sync-to-wrapper nil ,*editors-options*)))

(defun mcvs-help (global-options command-options args)
  (declare (ignore global-options command-options))
  (cond
    ((null args)
      (terpri)
      (write-line *usage*)
      (terpri))
    ((= (length args) 1)
      (let* ((command-name (first args))
	     (command (find command-name *mcvs-command-table* 
			    :key #'first
			    :test #'string=)))
	(when (null command)
	  (error "~a is not a recognized mcvs command." 
		 command-name))
	(let ((help-text (third command)))
	  (when (null help-text)

	    (error "sorry, no help available for ~a command."
		   command-name))
	  (terpri)
	  (write-line help-text)
	  (terpri))))
    (t (error "try \"mcvs help <name-of-command>\"."))))

; This is not very elegant but SBCL refuses to compile the
; cyclic dependency between *mcvs-command-table* and
; mcvs-help.
(push `("help" ,#'mcvs-help nil ,*help-options*) *mcvs-command-table*)

(defparameter *usage*
"Meta-CVS command syntax:

  mcvs [ global-options ] command [ command-options ] [ command-arguments ]

Global options:

  -H --help          Print this help and terminate. If a command is specified,
                     help specific to that command is printed instead.
  -Q                 Very quiet, generate output only for serious problems. (*)
  -q                 Somewhat quiet, some info messages suppressed. (*)
  -n                 Dry run; do not modify filesystem. (*)
  --debug            Verbose debug output; -Q and -q are ignored but still
                     passed to CVS.
  -r                 Make working files read-only. (@)
  -w                 Make new working files read-write (default). (@)
  -l                 Do not log cvs command in command history, but execute
                     it anyway. (@)
  -t                 Trace CVS execution. (@)
  -v --version       Display version information and terminate.
  -f                 CVS not to read ~/.cvsrc file. (@)
  -i script-name     Load a Lisp file and evaluate its top level forms,
                     allowing Meta-CVS to behave as an interpreter.
  --meta             Include metafiles such as MCVS/MAP in the set of files
                     to operate on.
  --metaonly         Operate only on metafiles.
  --nometa           Exclude metafiles from the set of files to operate on.
  --error-continue   Instead of interactive error handling, automatically 
                     continue all continuable errors.
  --error-terminate  Terminate with cleanup when an error happens instead
                     of interactive error handling.
  -T tempdir         Place temporary files in tempdir. (@)
  -e editor          Edit messages with editor. (*)
  -d root            Specify CVSROOT. (@)
  -z gzip-level      Specify compression level. (@)
  --up N             Escape out of N levels of sandbox nesting before executing
                     operation.

  Notes: (*) option processed by Meta-CVS and passed to CVS too.
         (@) option merely passed to CVS.

Commands:

  help               Obtain more detailed help for a specific command.
  create             Create new project from an existing file tree.
  grab               Take a snapshot of an external source tree, such
                     as a third-party release, and incorporate it into
                     the working copy. Tries to discover file moves.
  checkout (co)      Retrieve a Meta-CVS project from the repository to
                     create a working copy.
  export (ex)        Retrieve a Meta-CVS project without creating a 
                     working copy.
  add                Place files (or directories with add -R) under
                     version control.
  remove (rm)        Remove files or directories.
  move (mv)          Rename files and directories.
  link (ln)          Create a versioned symbolic link.
  update (up)        Incorporate latest changes from repository into 
                     working copy. 
  commit (ci)        Incorporate outstanding changes in the working copy
                     into the repository.
  diff               Compute differences between files in the working copy
                     and the repository or between revisions in the repository.
  tag                Associate a symbolic name with file revisions to create
                     an identifiable baseline.  By default, tags the
                     revisions that were last synchronized with the
                     directory. Note: tag -b creates a CVS branch,
                     it won't be a Meta-CVS branch with managed merges.
                     Consider the branch command instead!
  log                Display log information for files.
  status (stat)      Show current status of files.
  annotate           Perform a detailed analysis of files, showing the 
                     version information about every individual line of text.
  filt (fi)          Act as a text filter, which converts Meta-CVS F- file 
                     names to readable paths, according to the current mapping.
  remote-filt (rfi)  Remote version of filt, requires module name.
  branch             Create a managed branch. Meta-CVS managed branches keep 
                     track of what has been merged where, so users don't have
                     to track merges with tags at all.
  merge              Merge a managed branch to the current branch or trunk.
  remerge            Re-apply the most recent merge without changing any tags.
                     Useful when a merge goes bad so the local changes have
                     to be discarded and the merge done over again.
  list-branches (lb) List Meta-CVS managed branches.
  switch (sw)        Switch to a branch. With no arguments, switch to 
                     main trunk.
  remap              Force Meta-CVS to notice and incorporate moves and
                     deletions that were performed directly on the sandbox.
  purge              Execute a CVS remove on files that have been unmapped
                     with the remove command.
  restore            Restore files that have been deleted with the remove
                     command, but not purged. These appear in the lost+found
                     directory under cryptic names.
  prop               Manipulate properties.
                       prop --set <bool-prop-name> [ files ... ]
                       prop --clear <bool-prop-name> [ files ... ]
                       prop --value <prop-name> <new-value> [ files ... ]
                       prop --remove <prop-name> [ files ... ]
                     The ``exec'' property represents the execute permission
                     of a file.  More than one --set, --clear, --value 
                     or --remove may be specified before the files.
  watch              Manipulate per-file CVS watch settings.
                       watch --on [ files ... ]
                       watch --off [ files ... ]
                       watch --add <action> [ files ... ]
                       watch --remove <action> [ files ... ]
  watchers           See who is watching files.
  edit               Indicate the intent to edit a watched file.
  unedit             Retract the indication signaled by edit.
  editors            See who is editing files.
  sync-to-cvs        Synchronize tree in the direction of the CVS sandbox.
                     Useful when extending Meta-CVS with external scripts.
  sync-from-cvs      Synchronize CVS sandbox to the tree.
  convert            Convert a CVS module to a Meta-CVS project.
                     This requires direct filesystem access to the repository.
                     Caveat: this is a very blunt instrument.")

(defmacro with-open-file-ignore-errors ((var &rest open-args) &body forms)
  `(let ((,var (ignore-errors (open ,@open-args))))
     (unwind-protect
       (progn ,@forms)
       (when ,var (close ,var)))))

(defun mcvs-execute (args)
  (with-open-file-ignore-errors (*interactive-error-io* (parse-posix-namestring 
							  (ctermid))
							:direction :io
							:if-does-not-exist nil)
    (let ((*mcvs-error-treatment* (if *interactive-error-io*
				    :interactive
				    :terminate)))
      (unless *interactive-error-io*
	(chatter-info "unable to open terminal device ~a .~%" 
		      (ctermid))
	(chatter-info "interactive error handling disabled.~%"))
      (handler-bind ((error #'mcvs-error-handler))
	(multiple-value-bind (global-options global-args)
			     (parse-opt args *global-options*)
	  (setf global-options (filter-global-options global-options))

	  (when *print-usage*
	    (terpri)
	    (write-line *usage*)
	    (terpri)
	    (throw 'mcvs-terminate nil))

	  (when (not (first global-args))
	    (write-line "Meta-CVS requires a command argument." *error-output*)
	    (write-line "Use mcvs -H to view help." *error-output*)
	    (throw 'mcvs-terminate nil))

	  (let ((command (find (first global-args) *mcvs-command-table* 
			       :key #'first
			       :test #'string=)))
	    (when (not command)
	      (error "~a is not a recognized mcvs command." 
		     (first global-args)))
	    (destructuring-bind (name func help-text opt-spec) command
	      (declare (ignore name help-text))
	      (multiple-value-bind (command-options command-args)
				   (parse-opt (rest global-args) opt-spec)
		(funcall func global-options command-options command-args)))))))
    nil))

(defun mcvs-debug-shell ()
  (let ((counter 0)
        (*mcvs-error-treatment* :decline))
    (loop
      (format t "~&mcvs[~a]> " (incf counter))
      (let ((line (string-trim #(#\space #\tab) (read-line))))
	(restart-case
	  (cond
	    ((zerop (length line)))
	    ((string-equal line "exit")
	       (return-from mcvs-debug-shell))
	    ((char-equal (char line 0) #\!)
	       (print (eval (read-from-string (subseq line 1)))))
	    (t (mcvs-execute (split-words line #(#\space #\tab)))))
	  (debug () :report "Return to mcvs debug shell"
	    (terpri)))))))

#+clisp
(defun mcvs ()
  (exit (catch 'mcvs-terminate (or (mcvs-execute ext:*args*)
				   *mcvs-errors-occured-p*))))
