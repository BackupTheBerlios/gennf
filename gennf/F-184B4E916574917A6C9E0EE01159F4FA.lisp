;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "dirwalk")
(require "chatter")
(require "split")
(require "mapping")
(require "types")
(require "rcs-utils")
(provide "convert")

(defun remove-attic-component (path)
  (let ((split-path (nreverse (split-fields path "/")))
	(attic-p nil))
    (when (string= (first split-path) "Attic")
      (pop split-path)
      (setf attic-p t))
    (values (reduce #'(lambda (x y) (format nil "~a/~a" x y)) 
		    (nreverse split-path)
		    :initial-value ".")
	    attic-p)))

(defun classify-tags (tags)
  (let (version-tags branch-tags)
    (dolist (tag tags (values version-tags branch-tags))
      (destructuring-bind (tag-name tag-value) tag
	(if (search ".0." tag-value)
	  (push tag-name branch-tags)
	  (push tag-name version-tags))))))
	
(defun mcvs-convert (source-dir target-dir)
  (when (ignore-errors (stat target-dir))
    (error "a directory or file called ~a exists here already."
           target-dir))

  (multiple-value-bind (path created) 
                       (ensure-directories-exist (path-cat target-dir 
							   *mcvs-map-name*))
    (declare (ignore path))
    (if (not created) 
      (error "unable to create directory ~a." target-dir)))

  (let (filemap all-version-tags all-branch-tags attic-made)
    (current-dir-restore
      (chdir source-dir)
      (for-each-file-info (fi ".")
	(when (and (directory-p fi)
		   (path-equal (basename (file-name fi)) "CVS"))
	  (skip))
	(when (regular-p fi)
	  (let ((canon-name (canonicalize-path (file-name fi))))
	    (multiple-value-bind (suffix basename dir) 
				 (suffix canon-name #\,)
	      (multiple-value-bind (no-attic-dir attic-p)
				   (remove-attic-component (or dir "."))
		(when (and suffix (string= suffix "v"))
		  (let* ((no-attic-suffix-name 
			   (canonicalize-path (path-cat no-attic-dir basename)))
			 (f-name (mapping-generate-id :suffix (suffix basename)
						      :no-dir t))
			 (orig-rcs (path-cat source-dir canon-name))
			 (new-rcs (apply #'path-cat `(,target-dir
						      ,@(if attic-p '("Attic"))
						      ,(format nil "~A,v" 
							       f-name)))))
		    (in-original-dir
		      (when attic-p
			(unless attic-made
			  (ensure-directories-exist new-rcs)
			  (setf attic-made t)))
		      (chatter-info "hard linking ~a -> ~a~%" orig-rcs new-rcs)
		      (link orig-rcs new-rcs))
		    (push (make-mapping-entry :kind :file
					      :id (path-cat *mcvs-dir* f-name)
					      :path no-attic-suffix-name
					      :executable (executable-p fi))
			  filemap)
		    (with-open-file (f (parse-posix-namestring canon-name) 
				       :direction :input)
		      (chatter-info "scanning ~a~%" canon-name)
		      (let ((rcs-file (rcs-parse f)))
			(multiple-value-bind (version-tags branch-tags)
					     (classify-tags (rcs-admin-symbols (rcs-file-admin rcs-file)))
			  (setf all-version-tags (nunion all-version-tags 
							 version-tags 
							 :test #'string=))
			  (setf all-branch-tags (nunion all-branch-tags branch-tags 
							:test #'string=)))))))))))))

    (current-dir-restore
      (chdir target-dir)
      (chatter-info "writing ~a~%" *mcvs-map-name*)
      (mapping-write filemap *mcvs-map-name* :sort-map t)

      (chatter-info "writing ~a~%" *mcvs-types-name*)
      (with-open-file (f *mcvs-types-name* :direction :output)
	(prin1 nil f)
	(terpri f))

      (chatter-info "writing .cvsignore~%")
      (with-open-file (f (make-pathname :name ".cvsignore") :direction :output)
	(write-line *mcvs-map-local-name* f)
	(write-line *mcvs-displaced-name* f))

      (execute-program `("ci" "-mCreated by Meta-CVS convert operation."
			 "-t/dev/null" ,*mcvs-map-name* 
			 ,*mcvs-types-name* ".cvsignore"))
      (execute-program `("chmod" "ug+rw" ,(format nil "~A,v" *mcvs-map-name*)))

      (chatter-info "setting up version and branch tags in ~a, ~a and .cvsignore~%" 
		    *mcvs-map-name* *mcvs-types-name*)
      (unless (null all-version-tags)
	(execute-program-xargs '("rcs") 
			       (mapcar #'(lambda (tag)
					   (format nil "-n~A:1.1" tag))
				       all-version-tags)
			       (list *mcvs-map-name* *mcvs-types-name* 
				     ".cvsignore")))

      (let ((branch-counter 0))
	(unless (null all-branch-tags)
	  (execute-program-xargs '("rcs")
				 (mapcar #'(lambda (tag)
					     (format nil 
						     "-n~A:1.1.0.~A" 
						     tag (incf branch-counter 
							       2)))
					 all-branch-tags)
				 (list *mcvs-map-name* *mcvs-types-name*
				       ".cvsignore")))))))

(defun mcvs-convert-wrapper (cvs-options cvs-command-options mcvs-args)
  (declare (ignore cvs-options cvs-command-options))
  (if (/= (length mcvs-args) 2)
    (error "specify cvs source dir and new target dir."))
  (mcvs-convert (first mcvs-args) (second mcvs-args)))

(defconstant *convert-help*
"Syntax:

  mcvs convert source-cvs-module target-mcvs-module

Options:

  None.

Semantics:

  The convert command builds a Meta-CVS module directly out of the RCS files of
  a CVS module.  The source-cvs-module is the pathname to an existing
  module directory in the CVS repository containing an ordinary CVS module.
  The target-mcvs-module is the pathname of a new Meta-CVS module directory to
  be created. 
  
  The source and target paths have to be on the same filesystem volume.

  The chmod and rcs programs are required.

  The algorithm is extremely naive:
  
  - A list of the pathnames of the RCS files is collected, as the basis for
  creating the MAP file. The Attic directory components are removed from these
  paths, and the ,v suffixes are stripped.

  - The execute property of files is lifted from the permission bits on
  the RCS files.

  - The MAP,v file is created using the ``rcs ci'' command.

  - The F- files are generated as hard links to the RCS files, to save space
  and avoid the overhead of copying.

  - All of the RCS files are scanned to find version and branch tags.  Quite
  naively, the version tags are installed in the MAP file, all pointing to
  revision 1.1. The branch tags are installed in MAP, pointing to revisions
  1.1.0.2, 1.1.0.4, ...  This is a lame attempt to make it possible to check
  out past baselines. But note that the contents of MAP don't vary: only a
  single version node is generated with a fixed set of files. It is not taken
  into consideration that some of the CVS files may be deleted in the head
  revision or some branches.  Therefore, when the resulting Meta-CVS project is
  checked out, or when past versions are retrieved, there may be complaints
  from Meta-CVS about nonexistent files.

  The complaints about nonexistent files may be fixed at the tips of the
  main trunk or branches using the ``mcvs remap'' command which will purge
  the working MAP of entries for F- files for which no working copy is found.
  A commit will then commit the change so that subsequent work may continue
  without any more complaints.
 
  The hard linking of the original RCS objects under F- names means that any
  permission, ownership or time-stamp changes done in the CVS module will
  affect the content of the Meta-CVS module and vice versa. Destructive
  modifications to the file contents, ditto. Be careful!

  If the hard links make you nervous, do a deep copy of the module,
  using ``cp -a source-dir target-dir''.

  Note that CVS does not destructively manipulate RCS files. A commit
  or tagging operation creates a new RCS object which atomically replaces the
  old hard link. This means that a commit to a file in the Meta-CVS module will
  not affect the CVS module and vice versa.")
