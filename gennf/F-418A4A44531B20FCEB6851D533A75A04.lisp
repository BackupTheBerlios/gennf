;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "dirwalk")
(require "system")
(require "mapping")
(require "types")
(require "chatter")
(require "options")
(require "restart")
(provide "create")

(defun mcvs-create (module release &optional global-options command-options)
  (multiple-value-bind (path created) (ensure-directories-exist *mcvs-map*)
    (declare (ignore path))
    (if (not created) 
      (error "A ~a directory already exists here." *mcvs-dir*)))

  (let ((preserve-mcvs-dir nil))
    (unwind-protect 
      (progn
	(let (filemap types)
	  (chatter-debug "Mapping.~%")
	  
	  ;; Gather up list of files to import, and build up mapping,
	  ;; as well as list of suffixes (file types).
	  (for-each-file-info (fi ".")
	    (cond
	      ((regular-p fi)
		 (let* ((path (canonicalize-path (file-name fi)))
			(suffix (suffix (file-name fi)))
			(file (mapping-generate-id :suffix suffix)))
		   (chatter-info "~a <- ~a~%" file path)
		   (push (make-mapping-entry :kind :file
					     :id file
					     :path path
					     :executable (executable-p 
							   fi))
			 filemap)
		   (when suffix
		     (setf types (adjoin (list suffix :default) 
					 types :test #'equal)))))
	      ((symlink-p fi)
		 (let ((path (canonicalize-path (file-name fi)))
		       (id (mapping-generate-id :prefix "S-" :no-dir t)))
		   (chatter-info "~a <- ~a~%" id path)
		   (push (make-mapping-entry :kind :symlink
					     :id id
					     :path path
					     :target (readlink path))
			 filemap)))))
				

	  ;; Write out types to file and allow user to edit.
	  (setf types (types-let-user-edit types *mcvs-types*))

	  ;; Detect backup files or other crud written by 
	  ;; user's text editor.
	  (current-dir-restore
	    (chdir *mcvs-dir*) 
	    (let (crud)
	      (for-each-path (p ".")
		(let ((cp (canonicalize-path p)))
		  (unless (or (path-equal cp *mcvs-types-name*)
			      (path-equal cp *this-dir*))
		    (push cp crud))))
	      (when crud
		(setf preserve-mcvs-dir t)
		(super-restart-case
		  (error "Unexpected files found in ~a directory. (Text editor backups?)"
			 *mcvs-dir*)
		  (continue ()
		    :report "Delete the unexpected files."
		    (unwind))
		  (info ()
		    :report "List the names of the unexpected files."
		    (dolist (cp crud)
		      (write-line cp))))
		(dolist (cp crud)
		  (unlink cp))
		(setf preserve-mcvs-dir nil))))

	  ;; User has edited, so now we must honor all of the :IGNORE
	  ;; entries in the types, and remove the matching files from the
	  ;; mapping.
	  (setf filemap (types-remove-ignores types filemap))

	  ;; Create F-files by hard linking
	  (dolist (entry filemap)
	    (with-slots (kind id path) entry
	      (when (eq kind :file)
		(link path id))))

	  ;; Write out mapping.
	  (mapping-write filemap *mcvs-map* :sort-map t)

	  ;; Create .cvsignore file.
	  (with-open-file (f (make-pathname :directory `(:relative ,*mcvs-dir*) 
					    :name ".cvsignore")
			     :direction :output)
	    (write-line *mcvs-map-local-name* f)
	    (write-line *mcvs-displaced-name* f))

	  (loop
	    (restart-case
	      (current-dir-restore
		(chdir *mcvs-dir*) 
		(chatter-debug "Invoking CVS.~%")

		(if (not (execute-program `("cvs" ,@(format-opt global-options) 
					   "import" "-I" "!"
					   ,@(format-opt command-options)
					   ,@(types-to-import-wrapper-args types)
					   ,module "Created-by-Meta-CVS" ,release)))
		  (error "CVS import failed."))
		(return))
	      (retry ()
		:report "Try invoking CVS again.")))))
      (if preserve-mcvs-dir
	(chatter-info "not removing ~a directory~%" *mcvs-dir*)
	(progn 
	  (chatter-debug "removing ~a directory~%" *mcvs-dir*)
	  (delete-recursive *mcvs-dir*)))))
  (values))

(defun mcvs-create-wrapper (cvs-options cvs-command-options mcvs-args)
  (if (< (length mcvs-args) 2)
    (error "specify module and release tag."))
  (destructuring-bind (module release &rest superfluous) mcvs-args
    (when superfluous
      (error "specify only module and release tag."))
    (mcvs-create module release cvs-options cvs-command-options)))

(defconstant *create-help*
"Syntax:

  mcvs create [ options ] module-name release-tag

Options:

  -d                Use a file's modification time as time of creation.
  -k subst-mode     Set default RCS keyword substitution mode.
  -I ignore-spec    Specify files to ignore in addition to whatever
                    is specified interactively. May cause problems;
                    since Meta-CVS will map these files anwyay.
  -b branch-num     Vendor branch number for CVS import. Deprecated
                    brain-damage; you should never need this.
  -m \"text ...\"     Log message.
  -W wrap-spec      CVS wrappers specification line. Keep in mind that 
                    Meta-CVS preserves suffixes only; CVS sees a
                    name like \"F-D3BC...30D5.html\".
Semantics:

  The create command makes a new Meta-CVS module from the files and symbolic
  links in the current directory, and all of its subdirectories. To work with
  the newly created module, you must check it out to create a working copy.
  The release-tag symbolically identifies the original baseline.

  There are some interactive steps involved. If any of the files have
  suffixes, like .c or .html, Meta-CVS will identify and tabulate them. 
  A text editor will pop up presenting you with an opportunity to edit
  a symbolic specification that assigns to each file type a CVS keyword
  expansion mode.")
