;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "split")
;(require "mapping")
;(require "options")
;(require "update")
;(require "system")

(define-constant *branch-char* #\~)

(defun tags-from-cvs-log (stream)
"Parse stream which is assumed to be the output of a cvs log -h command
for a single file. Return two associative lists, one of version tags
and one of branch tags."
  (let (syms (state :initial))
    (loop
      (let ((line (read-line stream nil)))
	(when (null line)
          (return-from tags-from-cvs-log (nreverse syms)))
	(ecase state
	  ((:initial)
	    (if (string= line "symbolic names:")
	      (setf state :syms)))
	  ((:syms)
	    (cond 
	      ((and (not (zerop (length line)))
		    (char= (char line 0) #\tab))
		 (push (split-words line #(#\: #\space #\tab)) syms))
	      (t (setf state :final))))
	  ((:final)))))))

(defun parse-dir-sticky (sticky)
  (if (string= "" sticky)
    nil
    (let ((first-char (char sticky 0))
	  (rest-string (substring sticky 1)))
      (case first-char
	(#\T (list :branch rest-string))
	(#\D (list :date rest-string))
	(#\N (list :version rest-string))
	(otherwise (list :other sticky))))))

(defun parse-entries-sticky (sticky)
  (if (string= "" sticky)
    nil
    (let ((first-char (char sticky 0))
	  (rest-string (substring sticky 1)))
      (case first-char
	(#\T (list :tag rest-string))
	(#\D (list :date rest-string))
	(otherwise (list :other sticky))))))

(defun equal-sticky (left right)
  (cond
    ((eq left right) t)
    ((null left) nil)
    ((null right) nil)
    (t (destructuring-bind (type-left text-left) left
	 (destructuring-bind (type-right text-right) right
	   (and (equal text-left text-right)
		(or (eq type-left type-right)
		    (and (eq type-left :tag)
			 (member type-right '(:version :branch)))
		    (and (eq type-right :tag)
			 (member type-left '(:version :branch))))))))))

(defun read-cvs-entries ()
  (with-open-file (f "CVS/Entries" :direction :input :if-does-not-exist nil)
    (when (not f)
      (error "cannot read CVS/Entries"))
    (let (entries)
      (do ((line (read-line f nil) (read-line f nil)))
	  ((null line) (nreverse entries))
	  (let ((split (split-fields line #(#\/))))
	    (setf (first split)
		  (cond 
		    ((string= "" (first split)) :file)
		    ((string= "D" (first split)) :directory)
		    (t :other)))
	    (when (sixth split)
	      (setf (sixth split) (parse-entries-sticky (sixth split))))
	    (push split entries))))))

(defun same-tag-check (entries &optional directory-sticky-tag)
  (let ((file-entries (remove-if-not #'(lambda (x) (eq x :file))
				     entries
				     :key #'first)))
    (let ((first-tag (or directory-sticky-tag (sixth (first file-entries)))))
      (not (find-if-not #'(lambda (x) (equal-sticky x first-tag))
			file-entries :key #'sixth)))))

(defun what-are-we-sticky-to ()
  (with-open-file (f "CVS/Tag" :direction :input :if-does-not-exist nil)
    (if f
      (let ((contents (read-line f nil)))
	(if contents
	  (parse-dir-sticky contents))))))

(defun what-module-is-this ()
  (with-open-file (f "CVS/Repository" :direction :input)
    (read-line f)))

(defun where-is-the-repository ()
  (with-open-file (f "CVS/Root" :direction :input)
    (read-line f)))

(defun branch-tag-check (tag)
  (when (some #'(lambda (ch) (char= ch *branch-char*)) tag)
    (error "tag must not contain ~a character." *branch-char*))
  (when (string= tag "HEAD")
    (error "HEAD is a reserved symbol." *branch-char*)))

(defun mcvs-branch (global-options branch-name)
  (branch-tag-check branch-name)
  (in-sandbox-root-dir
    (let ((branchpoint-tag (format nil "~a~abranch-point" 
				   branch-name *branch-char*)))
      (chdir *mcvs-dir*)
      (chatter-debug "Invoking CVS.~%")
      (execute-program `("cvs" ,@(format-opt global-options) 
			 "tag" "-b" ,branch-name))
      (execute-program `("cvs" ,@(format-opt global-options) 
			 "tag" ,branchpoint-tag)))))

(defun mcvs-branch-wrapper (global-options command-options command-args)
  (declare (ignore command-options))
  (if (/= (length command-args) 1)
    (error "specify branch symbol")
  (mcvs-branch global-options (first command-args))))

(defun cvs-make-or-advance-tag (global-options tag &optional tag-what)
  (let ((module (what-module-is-this))
	(repo (where-is-the-repository)))
    (if (or (not (execute-program `("cvs" ,@(format-opt global-options)
				    "tag" "-d" ,tag ,*mcvs-map-name*)))
	    (not (execute-program `("cvs" ,@(format-opt global-options) 
				    "-d" ,repo "rtag" "-F"
				    ,@(if tag-what `("-r" ,tag-what))
				    ,tag ,module))))
      (error "CVS tagging operation failed."))))

(defun mcvs-merge (global-options command-options branch-name &key remerge-p)
  (branch-tag-check branch-name)
  (in-sandbox-root-dir
    (chdir *mcvs-dir*)
    (let ((branchpoint-tag (format nil "~a~abranch-point" 
				   branch-name *branch-char*))
	  (current-sticky (what-are-we-sticky-to))
	  this-branch
	  (symbols (with-input-from-program (s `("cvs" 
						 ,@(format-opt global-options)
						 "log" "-h" ,*mcvs-map-name*))
		     (tags-from-cvs-log s))))

      (when (not (or (null current-sticky) 
		     (eq (first current-sticky) :branch)))
	(error "working copy is currently updated to a non-branch tag."))

      (setf this-branch (or (second current-sticky) "HEAD"))

      (when (string= this-branch branch-name)
	(error "cannot merge branch to itself."))

      (let* ((even-merge-tag (format nil "~a~amerged-to-~a-0" branch-name 
				     *branch-char* this-branch))
	     (odd-merge-tag (format nil "~a~amerged-to-~a-1" branch-name 
				    *branch-char* this-branch))
	     (branch-tag-pos (position branch-name symbols
				       :key #'first :test #'string=))
	     (even-tag-pos (position even-merge-tag symbols 
				     :key #'first :test #'string=))
	     (odd-tag-pos (position odd-merge-tag symbols 
				    :key #'first :test #'string=))
	     (bp-tag-pos (position branchpoint-tag symbols
				   :key #'first :test #'string=))
	     from-tag to-tag)

	(when (not branch-tag-pos)
	  (error "unable to retrieve branch symbol ~a." branch-name))
	(when (not bp-tag-pos)
	  (error "this is not a Meta-CVS managed branch."))

	(cond
	  (remerge-p
	     (cond
	       ((and even-tag-pos odd-tag-pos) 
		  (if (< even-tag-pos odd-tag-pos)
		    (setf from-tag odd-merge-tag to-tag even-merge-tag)
		    (setf from-tag even-merge-tag to-tag odd-merge-tag)))
	       (odd-tag-pos
		  (setf from-tag branchpoint-tag to-tag odd-merge-tag))
	       (even-tag-pos
		  (setf from-tag branchpoint-tag to-tag even-merge-tag))
	       (t (error "no prior merge was done"))))
	  (t (cond
	       ((and even-tag-pos odd-tag-pos)
		  (if (< even-tag-pos odd-tag-pos)
		    (setf from-tag even-merge-tag to-tag odd-merge-tag)
		    (setf from-tag odd-merge-tag to-tag even-merge-tag)))
	       (even-tag-pos
		  (setf from-tag even-merge-tag to-tag odd-merge-tag))
	       (odd-tag-pos
		  (setf from-tag odd-merge-tag to-tag even-merge-tag))
	       (t (setf from-tag branchpoint-tag to-tag even-merge-tag)))
	     (cvs-make-or-advance-tag global-options to-tag branch-name)))
	(mcvs-update global-options `(("j" ,from-tag) ("j" ,to-tag)
                                  ,@command-options))))))

(defun mcvs-list-branches (global-options)
  (in-sandbox-root-dir
    (chdir *mcvs-dir*)
    (let ((symbols (with-input-from-program (s `("cvs" 
						 ,@(format-opt global-options)
						 "log" "-h" ,*mcvs-map-name*))
		      (tags-from-cvs-log s)))
	  (entries (read-cvs-entries))
	  (branchpoint-suffix (format nil "~abranch-point" *branch-char*))
	  (current-sticky (what-are-we-sticky-to)))

      (format t "currently on: ~a (~a)~%" 
	      (or (second current-sticky) "main trunk")
	      (case (first current-sticky)
		((:branch)
		   (if (find (format nil "~a~abranch-point"
				     (second current-sticky) *branch-char*) 
			     symbols :key #'first :test #'string=)
		     "managed branch"
		     "non-managed branch"))
		((:version)
		   "version tag")
		((:date)
		   "sticky date")
		((nil) "no sticky tag")))

      (when (not (same-tag-check entries current-sticky))
	(format t "warning: one or more files not on ~a~%" 
		(or (second current-sticky) "main trunk")))

      (format t "branch list: ~%")
      (dolist (symbol symbols)
	(let* ((tag (first symbol))
	       (offset (search branchpoint-suffix tag)))
	  (when (and offset
		     (> offset 0)
		     (= offset (- (length tag) (length branchpoint-suffix))))
	    (format t "~a~a~%" #\Tab (substring tag 0 offset))))))))

(defun mcvs-merge-wrapper (global-options command-options command-args)
  (when (/= (length command-args) 1)
    (error "specify source branch symbol."))
  (mcvs-merge global-options command-options (first command-args)))

(defun mcvs-remerge-wrapper (global-options command-options command-args)
  (when (/= (length command-args) 1)
    (error "specify source branch symbol."))
  (mcvs-merge global-options command-options (first command-args) :remerge-p t))

(defun mcvs-list-branches-wrapper (global-options command-options command-args)
  (declare (ignore command-options))
  (when (not (zerop (length command-args)))
    (error "command takes no arguments."))
  (mcvs-list-branches global-options))

(defun mcvs-switch-wrapper (global-options command-options command-args)
  (let ((up-opt (case (length command-args)
		  ((0) `("A"))
		  ((1) `("r" ,(first command-args)))
		  (otherwise 
		    (error "specify at most one branch tag.")))))
    (mcvs-update global-options `(,up-opt ,@command-options))))

(define-constant *branch-help*
"Syntax:

  mcvs branch branch-name

Options:

  none

Semantics

  A branch can sprout from any point in the repository history. The branch
  command makes a branch starting at the closest repository revisions of all
  files in the sandbox, and associates that branch with the given branch name,
  which must be unique among branch names and tags.

  A branch is a fork in the revision history of a project. When a project is
  created, it has one branch which is called the main trunk. Every branch has
  a tip, which consists of the latest committed revisions of the files.
  Committing changes advances the tip to include newer revisions, causing
  the superseded revisions to recede into the branch history. That is how
  the repository grows to include new material, without losing past versions.

  Branches are needed for two reasons: to isolate changes, and to create
  changes based on old work.

  Isolating changes from each other is important for managing the risks
  associated with making changes to software (known as ``change management'').
  Branches decouple the work of making the changes from the decisions about
  what version of the software those changes will be integrated into.  For
  example, branching allows developers to put only critical bugfixes into an
  upcoming software release, while continuing to develop new features for a
  future version after that release. This is done by creating a branch for the
  critical bugfixes, and then eventually making the release from that branch,
  while development takes place on the trunk.  The trunk also needs the
  critical bugfixes that are put into the release.  These fixes don't have to
  be done twice. Rather, the branch is merged to the trunk, which is a mostly
  automatic process, triggered by the invocation of the merge command.
  A branch can also be created to isolate risky experimental changes, so
  that their intergration can be delayed until they are stable, without
  the need to suspend the actual work of writing the changes.

  Secondly, a branch is needed when a change must be made based on file
  revisions that are no longer at the tip of their branch. Since commits happen
  only at the tip, when changes must be based on some historic version rather
  than the latest version, a branch is used. This mechanism allows developers
  to fix a bug in some old version of the software, and send that fix to the
  customer who doesn't want to, or cannot upgrade to the latest version.
  If that fix is pertinent to the latest version of the software, that branch
  can be merged to the trunk; even if the fixed version is very old, it's
  possible that the fix will merge with only a fraction of the effort that
  would be required to re-do the fix.

  Branches are only an important tool; making effective use of branching
  requires that the users understand, agree upon and follow an intelligent
  change management process.")
