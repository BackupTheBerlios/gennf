;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "dirwalk")
;(require "chatter")
;(require "sync")
;(require "options")
;(require "find-bind")
;(provide "checkout")

(defun mcvs-checkout (module &optional subdir cvs-options checkout-options
			     &key no-generate behave-like-export)
  (when subdir
    (when (path-absolute-p subdir)
      (error "subdirectory path must be relative"))
    (multiple-value-bind (canon-subdir out-of-bounds) 
			 (canonicalize-path subdir)
      (declare (ignore canon-subdir))
      (when out-of-bounds
	(error "subdirectory path ~a leads outside of module."
	       subdir))))
  (find-bind (:key #'first :test #'string= :take #'second) 
	     (cvs-checkout-options (dir "d" (or subdir module))) 
	     checkout-options
    (let ((checkout-dir (canonicalize-path dir))
	  path checkout-okay created-dir created-mcvs-dir)
      (multiple-value-setq (path created-dir) 
        (ensure-directories-exist (path-cat checkout-dir *mcvs-dir*)))
      (unwind-protect
	(current-dir-restore
	  (chdir checkout-dir)

	  (when (ignore-errors (stat *mcvs-dir*))
	    (error "directory ~a seems to be the root of an existing sandbox."
		   checkout-dir))

	  (chatter-debug "Invoking CVS.~%")
	  (unless
	    (execute-program `("cvs" ,@(format-opt cvs-options) 
			       ,(if behave-like-export "export" "checkout") 
			       "-d" ,*mcvs-dir*
			       ,@(format-opt cvs-checkout-options) ,module))
	    (error "CVS checkout failed."))

	  (unless (ignore-errors (stat *mcvs-dir*))
	    (error "checkout failed to create ~a directory." 
		   *mcvs-dir*))

	  (setf created-mcvs-dir t)
	  (mapping-write nil *mcvs-map-local*)
	  (if subdir
	    (displaced-path-write (concatenate 'string 
					       (canonicalize-path subdir)
					       *path-sep*)))
	  (unless no-generate
	    (in-sandbox-root-dir
	      (chatter-debug "Generating file structure.~%")
	      (mapping-update)))
	  (chatter-info "Checkout to directory ~a completed.~%" checkout-dir)
	  (setf checkout-okay t))
	(when (or behave-like-export (not checkout-okay))
	  (when created-mcvs-dir
	    (delete-recursive (path-cat checkout-dir *mcvs-dir*))))
	(unless checkout-okay
	  (when created-dir
	    (delete-recursive checkout-dir))))
      (values))))

(flet ((err () 
	 (error "specify module, and optional subdirectory")))
  (defun mcvs-checkout-wrapper (global-options command-options args)
    (when (< (length args) 1)
      (err))
    (destructuring-bind (module &optional subdir &rest superfluous) args
      (when superfluous
	(err))
      (mcvs-checkout module subdir global-options command-options)))

  (defun mcvs-export-wrapper (global-options command-options args)
      (when (< (length args) 1)
	(err))
      (destructuring-bind (module &optional subdir &rest superfluous) args
	(when superfluous
	  (err))
	(find-bind (:test #'string= :key #'first)
		   ((revision "r")
		    (date "D"))
		   command-options
	  (cond
	    ((not (or revision date))
	       (error "specify tag with -r or date with -D."))
	    ((and revision date)
	       (error "both -r and -D specified.")))

	  (mcvs-checkout module subdir global-options command-options 
			 :behave-like-export t)))))

(define-constant *checkout-help*
"Syntax:

  mcvs co [ options ] module-name [ subdirectory-path ]

Options:

  -f                Force a head revision match if tag or date is not found.
  -r revision       Check out specific revision or branch and make it sticky.
  -D date           Check out by date.
  -d dir            Check out into specified directory instead of creating
                    a directory based on the module name. 
  -k key-expansion  Specify RCS keyword expansion option.
  -j revision       Merge in the changes between current revision and rev.
                    Note that Meta-CVS has branch and merge commands; using
                    the -j options of checkout or update bypasses the
                    Meta-CVS merge system.

Semantics:

  The checkout command retrieves a module from Meta-CVS to form a working copy,
  also known as a ``sandbox'' in version control jargon. 

  By default, a subdirectory is created whose name is the same as the
  module-name. The module's directory structure is unfolded down there. An
  alternate directory can be specified with the -d option. Meta-CVS will
  try to create the checkout directory if it does not exist. Populating
  an existing directory is safe; Meta-CVS will stop if it encounters
  any conflicting local files.

  If the optional subdirectory-path parameter is specified, Meta-CVS will
  create a ``partial sandbox'', whose root directory is the specified
  path. This parameter is understood to be a relative path within the
  module's tree structure, resolved with respect to the root. For example
  if the module has a lib/zlib subdirectory, then specifying lib/zlib
  will create a sandbox whose root directory corresponds to lib/zlib.
  Files not under lib/zlib won't be visible in the sandbox. A nonexistent
  path can be specified; in that case the partial sandbox will be empty. Adding
  new files within the sandbox will cause the path to exist. For example,
  if the module contains no directory called lib/libdes it's still possible
  to check out that directory. Then adding a file called foo.c in the
  root directory of the sandbox will actually add a lib/libdes/foo.c file
  to the module.")

(define-constant *export-help*
"Syntax:

  mcvs export { -D date | -r revision } [ options ] 
       module-name [ subdirectory-path ]

Options:

  -f                Force a head revision match if tag or date is not found.
  -r revision       Check out specific revision or branch and make it sticky.
  -D date           Check out by date.
  -d dir            Check out into specified directory instead of creating
                    a directory based on the module name. 
  -k key-expansion  Specify RCS keyword expansion option.

Semantics:

  The export command is almost the same as the checkout command. Unlike
  checkout, export does not create a MCVS subdirectory, and so the result is
  not a working copy.  It requires that a document baseline be specified by
  symbolic revision or date.  Lastly, it does not accept the -j option to
  specify merging (but this way of merging on checkout is deprecated in
  Meta-CVS; do not use it with managed branches).")
