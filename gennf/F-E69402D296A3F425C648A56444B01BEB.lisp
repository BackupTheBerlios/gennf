;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "mapping")
(require "chatter")
(require "find-bind")
(provide "remove")

(defun mcvs-remove (recursivep files &key no-sync)
  (when (null files)
    (return-from mcvs-remove (values)))

  (in-sandbox-root-dir
    (let (files-to-remove (filemap (mapping-read *mcvs-map*)))

      (chatter-debug "Unmapping.~%")
      (dolist (file files)
	(can-restart-here ("Continue unmapping files.")
	  (let* ((full-name (sandbox-translate-path file))
		 (abs-name (canonicalize-path 
			      (real-to-abstract-path full-name)))
	   	 (entries (mapping-prefix-matches filemap abs-name)))
	    (cond
	      ((path-prefix-equal *mcvs-dir* full-name)
		 (error "cannot remove ~a: path is in a reserved Meta-CVS area."
			full-name))
	      ((and (second entries) (not recursivep))
		 (error "cannot remove ~a: it is a directory, use -R to remove." 
			full-name))
	      ((not entries)
		 (if (exists full-name)
		   (error "cannot remove ~a: it is local, not versioned under Meta-CVS."
			  full-name)
		   (error "cannot remove ~a: it does not exist." full-name)))
	      (t (setf files-to-remove (nconc files-to-remove entries)))))))

      (when files-to-remove
	;; Removed files might have unsynchronized local edits, which
	;; will be irretrievably lost if we don't synchronize.
	;; But the grab command does not need this, hence no-sync option.
	(chatter-debug "Synchronizing.~%")
	(unless no-sync
	  (mapping-synchronize :direction :left))
	(let ((new-filemap (set-difference filemap files-to-remove 
			    		   :test #'mapping-same-id-p)))
	  (mapping-write new-filemap *mcvs-map* :sort-map t))

	(chatter-debug "Updating file structure.~%")
	(mapping-update :no-delete-removed no-sync))))
  (values))

(defun mcvs-remove-wrapper (cvs-options cvs-command-options mcvs-args)
  (declare (ignore cvs-options))
  (find-bind (:test #'string= :key #'first)
	     ((recursivep "R"))
	     cvs-command-options
    (mcvs-remove recursivep mcvs-args)))

(defconstant *remove-help*
"Syntax:

  mcvs remove [ options ] objects ...

Options:

  -R                Recursive behavior: recursively remove objects
                    in subdirectories. By default, trying to remove
                    a subdirectory signals a continuable error.

Semantics:

  The remove command removes objects from the mapping. To propagate
  the removal to the repository, a commit operation must be invoked.

  Removed files are not actually subject to a CVS-level erasure; they are
  merely removed from the map, but still exist in the MCVS subdirectory. Their
  local modifications are not lost. To actually remove files from CVS,
  use the purge command.  Removed files that have not been purged can be
  recovered via the restore command which re-creates mapping entries for them
  in the lost+found directory under machine-generated names; they can then be
  renamed to more appropriate names. Symbolic links cannot be restored;
  they exist as mapping entries only.")
