;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "system")
;(require "mapping")
;(require "chatter")
;(require "restart")
;(provide "move")

(defun source-check (expansion source)
  (let ((real (abstract-to-real-path source)))
    (when (path-prefix-equal *mcvs-dir* real)
       (error "source path ~a is in a reserved Meta-CVS area." real))
    (when (path-equal *this-dir* real)
       (error "cannot move the sandbox root directory."))
    (when (not expansion)
      (if (exists real)
	(error "~a is local, not versioned under Meta-CVS." real)
	(error "~a does not exist." real)))))

(defun simple-rename (filemap source dest-file)
  (let ((dir-expansion (mapping-extract-paths 
			 (mapping-prefix-matches filemap source))))
    (source-check dir-expansion source)
    (mapping-rename-files filemap dir-expansion source dest-file)))

(defun simple-move-to-dir (filemap source dest-dir)
  (let ((dir-expansion (mapping-extract-paths 
			 (mapping-prefix-matches filemap source))))
    (source-check dir-expansion source)
    (multiple-value-bind (base dir) (basename source)
      (if dir
	(if (path-equal dest-dir *this-dir*)
	  (mapping-rename-files filemap dir-expansion source base)
	  (mapping-rename-files filemap dir-expansion dir dest-dir))
	(mapping-rename-files filemap dir-expansion source
	  (canonicalize-path (path-cat dest-dir base)))))))

(defun move-guts (filemap sources destination)
  (let* ((two-args (null (second sources)))
	 (destination-trailing-slash (string= (char destination
						    (1- (length destination)))
					      *path-sep*))
	 (dest-real-path (abstract-to-real-path destination))
	 (destination-file-object (no-existence-error (stat dest-real-path)))
	 (destination-file-exists (or (mapping-lookup filemap destination)
				      (and destination-file-object
					   (not (directory-p 
						 destination-file-object)))))
	 (destination-dir-exists (and (not destination-file-exists)
				      (or (mapping-prefix-lookup filemap 
								 destination)
					  (directory-p destination-file-object)
					  destination-trailing-slash))))
    (if two-args
      (if destination-dir-exists
	(simple-move-to-dir filemap (first sources) destination)
	(simple-rename filemap (first sources) destination))
      (if destination-file-exists
	(error "cannot move multiple to ~a." destination)
	(let ((skipped-all t))
	  (dolist (source sources filemap)
	    (can-restart-here ("Skip ~a and continue renaming." source)
	      (setf filemap (simple-move-to-dir filemap source destination))
	      (setf skipped-all nil)))
	  (when skipped-all
	    (error "skipped all move sources."))
	  filemap)))))

(defun mcvs-move (args)
  (when (< (length args) 2)
    (error "requires at least two arguments."))
  (in-sandbox-root-dir
    (chatter-debug "Renaming.~%")
    (let ((filemap (mapping-read *mcvs-map*))
	  (sources (mapcar #'real-to-abstract-path 
			   (mapcar #'sandbox-translate-path (butlast args))))
	  (destination (canonicalize-path
			 (real-to-abstract-path 
			   (sandbox-translate-path (first (last args)))))))

      (let ((dest-real (abstract-to-real-path destination)))
	(when (path-prefix-equal *mcvs-dir* dest-real)
	  (error "destination path ~a is in a reserved Meta-CVS area." 
		 dest-real)))

      (let ((edited-filemap (move-guts filemap sources destination))
	    (restore-map t))
	(setf edited-filemap (sort edited-filemap
				   #'string< :key #'mapping-entry-id))
	(when (equal-filemaps edited-filemap filemap)
	  (error "useless move of an object onto itself"))

	;; In case a move clobbers some object that has local edits,
	;; we need to synchronize it to the MCVS directory.
	(chatter-debug "Synchronizing.~%")
	(mapping-synchronize :direction :left)

	(unwind-protect
	  (progn
	    (mapping-write edited-filemap *mcvs-map* :sort-map nil)
	    (chatter-debug "Updating file structure.~%")
	    (when (mapping-update)
	      (setf restore-map nil)))
	  (when restore-map
	    (chatter-terse "Undoing changes to map.~%")
	    (mapping-write filemap *mcvs-map*)))))
  (values)))

(defun mcvs-move-wrapper (cvs-options cvs-command-options mcvs-args)
  (declare (ignore cvs-options cvs-command-options))
  (mcvs-move mcvs-args))

(define-constant *move-help*
"Syntax:

  mcvs move objects ... destination

Options:

  none

Semantics:

  The move command changes the names of versioned objects, resulting in a local
  edit of the map. Like any other local change, a move is not published to the
  repository until it is committed.

  A move which affects the last component of a path is known as a rename; the
  object appears to stay in the same directory, but its name changes. A move
  affecting one or more of the other components appears to be a relocation.
  Both of these are the same thing to the software.

  The move command relocates only those files which are versioned in Meta-CVS;
  it does not act upon local files. However, files can be moved into local
  directories. Files can also be moved such that they clobber local files.

  The behavior of the command very convenient, obeying the following rules:

  - If the destination is an existing directory in the sandbox,
    then the pathnames of the objects are renamed such that the objects are
    relocated into that directory. If any of the source objects
    are directories, then they are moved.

  - If the destination is an existing file, then there must be exactly one
    source argument; two or more objects cannot be moved to a non-directory
    object. If that object is a local file, then the move produces an error,
    which can be interactively resolved in favor of clobbering the file.
    If the object is a Meta-CVS versioned file, then it is silently removed
    as if by the remove command (which means that it is not lost, merely
    unlinked from the map).

  - If the destination does not exist, then it is deemed to be a directory
    name if there are two or more source arguments, or to be a non-directory
    name if there is one source argument. In the first case, the directory
    is automatically created; there is no need for mkdir as with the Unix
    mv command, so this is a nice convenience.

  - If a move causes any directory to become empty, meaning that it contains
    no local files, or Meta-CVS versioned files, that directory is removed.
    This rule is applied recursively all the way up to the sandbox root: if
    removing a directory causes its parent to become empty, that parent is
    removed and so forth. Remember, empty directories have no representation
    in Meta-CVS, and this is another way in which this turns out to be
    a convenience. Directories that are not empty are not removed, even
    if they contain only local files not known to the version control system.
  
  - A directory can be moved into itself or to a subdirectory of itself.
    The reader is invited to experiment to see how this works. The Unix
    mv command disallows this, as does the underlying rename() system
    call, so this is a third convenience.

  - The root directory of a full or partial sandbox cannot be a source
    argument.")
