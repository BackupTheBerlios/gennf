;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "mapping")
(require "chatter")
(require "dirwalk")
(require "seqfuncs")
(require "options")
(require "types")
(provide "add")

(defun mcvs-add (recursivep cvs-options add-options files)
  (in-sandbox-root-dir
    (let* ((filemap (mapping-read *mcvs-map*))
	   (saved-filemap (copy-list filemap))
	   (types-exists (exists *mcvs-types*))
	   (types (and types-exists (types-read *mcvs-types*)))
	   new-map-entries new-types)

      (chatter-debug "Mapping.~%")

      (dolist (file files)
	(let (expanded-paths)
	  (can-restart-here ("Continue processing arguments after ~a." file)
	    (if recursivep 
	      (for-each-path (full-name (sandbox-translate-path file))
		(push (canonicalize-path full-name) expanded-paths))
	      (push (sandbox-translate-path file) expanded-paths)))
	  (nreverse expanded-paths)

	  (dolist (full-name expanded-paths)
	    (can-restart-here ("Continue mapping files.")
	      (let ((abs-name (real-to-abstract-path full-name))
		    (file-info (stat full-name)))
		(cond
		  ((path-prefix-equal *mcvs-dir* full-name)
		    (error "cannot add ~a: path is in a reserved Meta-CVS area."
			   full-name))
		  ((mapping-lookup filemap abs-name)
		    (chatter-info "~a already added.~%" full-name))
		  ((directory-p file-info)
		    (when (not recursivep)
		      (error "cannot add ~a: it is a directory, use -R to add." full-name)))
		  ((regular-p file-info)
		    (let* ((suffix (suffix full-name))
			   (f-file (mapping-generate-id :suffix suffix)))
		      (when suffix
			    (setf new-types (adjoin (list suffix :default) 
						    new-types :test #'equal)))
		      (push (make-mapping-entry :kind :file
						:id f-file
						:path abs-name
						:executable (executable-p 
							      file-info))
			    new-map-entries)))
		  ((symlink-p file-info)
		    (let ((id (mapping-generate-id :no-dir t :prefix "S-")))
		      (push (make-mapping-entry :kind :symlink
						:id id
						:path abs-name
						:target (readlink full-name))
			    new-map-entries)))
		  (t
		    (error "cannot add ~a: not regular file or symlink." 
			   full-name))))))))

      (setf new-types (set-difference 
			new-types types :key #'first :test #'string=))

      (let ((*dry-run-option* nil))
	(unwind-protect
	  (setf new-types (types-let-user-edit new-types *mcvs-new-types*))
	  (ignore-errors (unlink *mcvs-new-types*))))

      (setf new-map-entries (types-remove-ignores new-types new-map-entries))
      (setf new-map-entries (types-remove-ignores types new-map-entries))

      (when new-map-entries
	(dolist (map-entry new-map-entries)
	  (with-slots (kind id path) map-entry
	    (push map-entry filemap)
	    (let ((real-name (abstract-to-real-path path)))
	      (chatter-info "mapping ~a <- ~a~%" id real-name)
	      (if (eq kind :file)
		(link real-name id)))))

	(mapping-write filemap *mcvs-map* :sort-map t)

	(when (setf types (append types new-types))
	  (types-write types *mcvs-types*))

	(setf new-map-entries (mapping-extract-kind new-map-entries :file))

	(let ((add-commands (types-make-cvs-adds types new-map-entries))
	      (restore-needed t))
	  (unwind-protect
	    (loop
	      (restart-case 
		(current-dir-restore 
		  (chdir *mcvs-dir*) 
		  (chatter-debug "Invoking CVS.~%")
		  (dolist (add-args add-commands)
		    (when (not (execute-program `("cvs" ,@(format-opt cvs-options)
						  "add" ,@(format-opt add-options)
						  ,@add-args)))
		      (error "CVS add failed.")))
		  (when (and types (not types-exists) (not *dry-run-option*))
		    (when (not (execute-program `("cvs" ,@(format-opt cvs-options)
						  "add" ,*mcvs-types-name*)))
		      (error "CVS add failed.")))
		  (setf restore-needed nil)
		  (return))
		(retry () 
		  :report "Try invoking CVS again.")))
	    (when restore-needed
	      (chatter-terse "Undoing changes to map.~%")
	      (mapping-write saved-filemap *mcvs-map*)
	      (ignore-errors
		(dolist (entry new-map-entries)
		  (unlink (mapping-entry-id entry)))))))

	(chatter-debug "Updating file structure.~%")
	(mapping-update))))
  (values))

(defun mcvs-add-wrapper (cvs-options cvs-command-options mcvs-args)
  (multiple-value-bind (recursivep rest-add-options) 
		       (separate "R" cvs-command-options 
				     :key #'first :test #'string=)
    (mcvs-add recursivep cvs-options rest-add-options mcvs-args)))

(defconstant *add-help*
"Syntax:

  mcvs add [ options ] objects ...

Options:

  -R                Recursive behavior: recursively add the contents
                    of all objects that are directories. By default,
                    trying to add a directory signals a continuable error.
  -m \"text ...\"     Use the specified text for the creation message.
  -k key-expansion  Add the file with the specified RCS expansion mode.

Semantics:

  The add command brings local filesystem objects under version control.
  The changes are not immediately incorporated into the repository; rather,
  the addition a local change that is ``scheduled'' until the next commit
  operation.
  
  Objects that can be added are files and symbolic links. Directories are not
  versioned objects in Meta-CVS; instead, files and symbolic links have a
  pathname property which gives rise to the existence of directories in the
  sandbox. The only significant consequence of this design choice is that empty
  directories have no direct representation in Meta-CVS.

  If any added files have suffixes that were not previously added to the
  project before, Meta-CVS will pop up a text editor to allow you to edit
  a specification that assigns to each new file type its CVS expansion
  mode.")
