;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "dirwalk")
(require "mapping")
(provide "remap")

(defun mcvs-remap ()
  (in-sandbox-root-dir
    (let* ((old-mapping (mapping-read *mcvs-map-local*))
	   (cvs-mapping (mapping-read *mcvs-map*))
	   (inode-hash (make-hash-table :test #'eql))
	   (new-mapping (remove-if #'(lambda (entry)
				       (with-slots (path kind) entry
					 (and (eq kind :file)
					      (real-path-exists path))))
				   old-mapping)))
      (restart-case
	(when (not (equal-filemaps old-mapping cvs-mapping))
	  (error "local and repository mappings differ."))
	(continue () :report "remap anyway, clobbering repository mapping"))
      (dolist (entry old-mapping)
	(with-slots (id kind) entry
	  (when (eq kind :file)
	    (let ((file-info (no-existence-error (stat id))))
	      (unless file-info
		(restart-case
		  (error "~a does not exist." id)
		  (continue () :report "Remove it from the map.")))
	      (when file-info
		(setf (gethash (inode file-info) inode-hash) entry))))))
      (for-each-file-info (fi ".")
	(let* ((path (canonicalize-path (file-name fi)))
	       (abs-path (real-to-abstract-path path)))
	  (cond 
	    ((regular-p fi)
	      (let ((entry (gethash (inode fi) inode-hash)))
		(when entry
                  (let ((new-entry (copy-mapping-entry entry)))
		    (setf (mapping-entry-executable new-entry)
			  (executable-p fi))
                    (setf (mapping-entry-path new-entry) abs-path)
		    (push new-entry new-mapping)
		    (setf (gethash (inode fi) inode-hash) nil)))))
	    ((symlink-p fi)
	      (chatter-info "skipping symbolic link ~a.~%" path))
	    ((directory-p fi)
	      (when (path-equal path *mcvs-dir*)
		(skip))))))
      (mapping-write new-mapping *mcvs-map-local* :sort-map t)
      (mapping-write new-mapping *mcvs-map* :sort-map t))))

(defun mcvs-remap-wrapper (global-options command-options args)
  (declare (ignore global-options command-options))
  (when args
    (error "command takes no arguments."))
  (mcvs-remap))
