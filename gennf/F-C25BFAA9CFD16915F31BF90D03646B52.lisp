;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "mapping")
(require "chatter")
(provide "link")

(defun mcvs-link (target name)
  (when (or (string= "" target)
	    (string= "" name))
    (error "empty path names are invalid."))
  (in-sandbox-root-dir
    (let* ((mapping (mapping-read *mcvs-map*))
	   (real-name (sandbox-translate-path name))
	   (abs-name (real-to-abstract-path real-name))
	   (trailing-slash (string= (char name (1- (length name))) *path-sep*))
	   (file-info (no-existence-error (stat real-name)))
	   (in-map (mapping-lookup mapping abs-name))
	   (prefix-in-map (mapping-prefix-lookup mapping abs-name))
	   (is-non-dir (or in-map (and file-info (not (directory-p file-info)))))
	   (is-dir (and (not is-non-dir)
			(or prefix-in-map (directory-p file-info) trailing-slash)))
	   (need-new-entry t))
      (when (path-prefix-equal *mcvs-dir* abs-name)
	 (error "path ~a is in a reserved Meta-CVS area." abs-name))

      ;; In case a link clobbers some object that has local edits,
      ;; we need to synchronize it to the MCVS directory.
      (chatter-debug "Synchronizing.~%")
      (mapping-synchronize :direction :left)

      (flet ((edit-map (entry)
	       (with-slots (path kind (tgt target)) entry
		 (chatter-debug "Clobbering existing object ~a.~%" path)
		 (unlink (abstract-to-real-path path))
		 (cond
		   ((eq kind :symlink)
		      (chatter-debug "Editing existing link.~%")
		      (setf tgt target)
		      (setf need-new-entry nil)
		      (symlink target (abstract-to-real-path path)))
		   (t (setf mapping
			    (remove entry mapping 
				    :test #'eq))))))
	     (make-new-entry (abs-name)
	       (when need-new-entry
		 (chatter-debug "Making new symlink entry ~a.~%" abs-name)
		 (push (make-mapping-entry :kind :symlink
					   :id (mapping-generate-id :no-dir t :prefix "S-")
					   :path abs-name
					   :target target)
		       mapping))))

	(cond
	  (in-map 
	     (edit-map in-map)
	     (make-new-entry abs-name))
	  (is-dir
	     (let* ((base (basename target))
		    (real-name (canonicalize-path (path-cat real-name base)))
		    (abs-name (canonicalize-path (path-cat abs-name base)))
		    (name (path-cat name base))
		    (file-info (no-existence-error (stat real-name)))
		    (in-map (mapping-lookup mapping abs-name))
		    (prefix-in-map (mapping-prefix-lookup mapping abs-name))
		    (is-non-dir (or in-map (and file-info (not (directory-p file-info)))))
		    (is-dir (and (not is-non-dir)
				 (or prefix-in-map (directory-p file-info)))))
	       (when is-dir
		  (error "~a is a directory." name))
	       (when in-map
		 (edit-map in-map))
	       (make-new-entry abs-name)))
	  (t (make-new-entry abs-name))))
      (mapping-write mapping *mcvs-map* :sort-map t)
      (chatter-debug "Updating file structure.~%")
      (mapping-update)))
  (values))

(defun mcvs-link-wrapper (global-options command-options args)
  (declare (ignore global-options command-options))
  (when (/= (length args) 2)
    (error "specify link target and link name."))
  (mcvs-link (first args) (second args)))

(defconstant *link-help*
"Syntax:

  mcvs link target-path name

Semantics:

  Create a symbolic link with the given name, containing the target path.
  Note the braindamaged reverse syntax, which is deliberately consistent
  with the Unix ``ln'' command. To make the symbolic link @foo -> bar,
  use ``mcvs ln bar foo''.

  Another way to add links is to create a symlink, and then use the add
  command.")
