;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "dirwalk")
;(require "system")
;(require "options")
;(require "sync")
;(require "chatter")
;(require "restart")
;(require "seqfuncs")
;(provide "mapping")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant *mcvs-dir* "MCVS")
  (define-constant *mcvs-map-name* "MAP")
  (define-constant *mcvs-map-local-name* "MAP-LOCAL")
  (define-constant *mcvs-displaced-name* "DISPLACED"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant *mcvs-map* #.(path-cat *mcvs-dir* *mcvs-map-name*))
  (define-constant *mcvs-map-local* #.(path-cat *mcvs-dir* *mcvs-map-local-name*))
  (define-constant *mcvs-displaced* #.(path-cat *mcvs-dir* *mcvs-displaced-name*)))

(defvar *displaced-path-prefix* nil)
(defvar *displaced-path-length* nil)

(defun mcvs-locate ()
  (let ((current-dir (split-fields (getcwd) *path-sep*))
	(escape-levels *nesting-escape-option*))
    (dotimes (i (length current-dir) nil)
      (let* ((path-components (butlast current-dir i))
	     (path-string (reduce #'path-cat path-components)))
	(when (and (ignore-errors (stat (path-cat path-string *mcvs-dir*)))
		   (zerop (prog1 escape-levels (decf escape-levels))))
	  (when (> *nesting-escape-option* 0)
	    (chatter-info "using sandbox ~a~%" path-string))
	  (chdir path-string)
	  (return (if (zerop i)
		    "."
		    (reduce #'path-cat (last current-dir i)))))))))

(declaim (inline real-path-exists abstract-to-real-path 
		 real-to-abstract-path))

(defun real-path-exists (path)
  (or (null *displaced-path-prefix*)
      (path-prefix-equal *displaced-path-prefix* path)))

(defun abstract-to-real-path (path)
  (if *displaced-path-length*
    (if (or (= (length path) (1- *displaced-path-length*)) 
	    (= (length path) *displaced-path-length*))
      *this-dir*
      (substring path *displaced-path-length*))
    path))

(defun real-to-abstract-path (path)
  (if *displaced-path-prefix*
    (concatenate 'string *displaced-path-prefix* path)
    path))

(defmacro in-sandbox-root-dir (&body forms)
  (let ((downpath-sym (gensym "DOWNPATH-")))
   `(current-dir-restore 
      (let ((,downpath-sym (mcvs-locate))) 
	(when (not ,downpath-sym)
	  (error "could not locate ~a directory." *mcvs-dir*))
	(let* ((*displaced-path-prefix* (displaced-path-read))
	       (*displaced-path-length* (if *displaced-path-prefix*
					  (length *displaced-path-prefix*))))
	  (flet ((sandbox-translate-path (in-path)
		   (multiple-value-bind (out-path out-of-bounds) 
					(canonicalize-path
					  (if (path-absolute-p in-path)
					    (path-cat *this-dir* in-path)
					    (path-cat ,downpath-sym in-path)))
		      (if out-of-bounds
			(error "path ~a is not within sandbox." out-path)
			out-path))))
	    (symbol-macrolet ((sandbox-down-path ,downpath-sym))
	      ,@forms)))))))

(defstruct mapping-entry
  (kind :file)
  (id "")
  (path "")
  (target "")
  (executable nil)
  (raw-plist nil))

(defun equal-mapping-entries (left right)
  (and (eq (mapping-entry-kind left) (mapping-entry-kind right))
       (string= (mapping-entry-id left) (mapping-entry-id right))
       (path-equal (mapping-entry-path left) (mapping-entry-path right))
       (equal (mapping-entry-target left) (mapping-entry-target right))))

(defun equal-filemaps (left right)
  (let ((same t))
    (mapc #'(lambda (le re)
	      (setf same (and same (equal-mapping-entries le re))))
	    left right)
    same))

(defun mapping-entry-parse-plist (entry)
  (with-slots (executable raw-plist) entry
    (destructuring-bind (&key exec &allow-other-keys) 
			raw-plist
      (setf executable exec)))
  (values))

(defun mapping-generate-id (&key no-dir (suffix "") (prefix "F-"))
  (format nil "~a~a~32,'0X~a" 
	  (if no-dir "" (concatenate 'string *mcvs-dir* *path-sep*))
	  prefix
	  (guid-gen) 
	  (if (or (null suffix) (string= "" suffix))
	    ""
	    (concatenate 'string "." suffix))))

(defun mapping-extract-kind (filemap kind)
  (remove-if-not #'(lambda (entry-kind) 
		     (eq entry-kind kind))
		 filemap
		 :key #'mapping-entry-kind))

(declaim (inline mapping-extract-paths))
(defun mapping-extract-paths (filemap)
  (mapcar #'mapping-entry-path filemap))

(defun mapping-lookup (filemap path)
  (find path filemap :test #'path-equal :key #'mapping-entry-path))

(defun mapping-prefix-lookup (filemap prefix)
  (if (path-equal *this-dir* prefix)
    (first filemap)
    (find prefix filemap :test #'path-prefix-equal :key #'mapping-entry-path)))

(defun mapping-prefix-matches (filemap path)
  (if (path-equal *this-dir* path)
    filemap
    (remove-if-not #'(lambda (entry) 
		       (path-prefix-equal path (mapping-entry-path entry))) 
		   filemap)))

(defun mapping-same-id-p (entry-one entry-two)
  (string= (mapping-entry-id entry-one) (mapping-entry-id entry-two)))

(defun mapping-same-path-p (entry-one entry-two)
  (path-equal (mapping-entry-path entry-one) (mapping-entry-path entry-two)))

(defun mapping-rename-files (filemap file-list old-prefix new-prefix)
"Returns a new filemap, in which the pathames in the list file-list are edited
by replacing the old-prefix with the new-prefix. If any path thus created
matches an existing map entry, that map entry is removed. The sorting order
of the map is not preserved."
  (flet ((combine (prefix path)
	   (if (string= path "")
	     prefix
	     (canonicalize-path (path-cat prefix path)))))
    (let* ((op-len (length old-prefix))
	   (delete-map (mapcan #'(lambda (entry)
				   (with-slots (path) entry
				     (if (and (member path file-list 
						      :test #'path-equal)
					      (path-prefix-equal old-prefix
					                         path))
				       (list entry)))) filemap))
	   (replace-map (mapcan #'(lambda (entry)
				    (with-slots (path) entry
				      (let ((new-entry (copy-mapping-entry
							 entry)))
					(setf (mapping-entry-path new-entry)
					      (combine new-prefix
						       (subseq path op-len)))
					(list new-entry))))
				 delete-map)))
    (append 
      (set-difference 
        (set-difference filemap delete-map :test #'mapping-same-path-p)
	replace-map :test #'mapping-same-path-p)
      replace-map))))

(defun malformed-map ()
  (error "malformed map (merge conflicts?): correct and run mcvs update."))

(defun mapping-dupe-check (filemap)
"Signals an error condition if the filemap contains duplicate paths or
duplicate objects. Otherwise returns the filemap, sorted by path."
  (let ((dupes)
	(id-hash (make-hash-table :test #'equal))
	(path-hash (make-hash-table :test #'equal)))
    (dolist (entry filemap)
      (if (gethash (mapping-entry-id entry) id-hash)
	(push entry dupes)
	(setf (gethash (mapping-entry-id entry) id-hash) entry))
      (if (gethash (mapping-entry-path entry) path-hash)
	(push entry dupes)
	(setf (gethash (mapping-entry-path entry) path-hash) entry)))
    (when dupes
      (dolist (dupe dupes)
	(chatter-terse "duplicate ~a -> ~a~%" 
		       (mapping-entry-id dupe) (mapping-entry-path dupe)))
      (error "duplicates in map: correct and run mcvs update.")))
  filemap)

(defun mapping-convert-old-style-in (raw-filemap)
"Converts old-style Meta-CVS file mapping to a list of mapping-entry
structures."
  (mapcar #'(lambda (item)
	      (when (or (not (consp item))
			(not (and (stringp (first item))
				  (stringp (second item)))))
		(malformed-map))
	      (make-mapping-entry :kind :file 
				  :id (first item)
				  :path (second item)))
	  raw-filemap))

(defun mapping-convert-in (raw-filemap)
"Converts a Meta-CVS filemap as read from a file into its internal
representation---a list of mapping-entry structures."
  (mapcar #'(lambda (item)
	      (when (or (not (consp item))
			(not (and (keywordp (first item))
				  (stringp (second item)))))
		(malformed-map))
	      (case (first item)
		((:file)
		   (let ((entry (make-mapping-entry :kind :file
						    :id (second item)
						    :path (third item)
						    :raw-plist (fourth item))))
		     (when (fourth item)
		       (mapping-entry-parse-plist entry))
		     entry))
		((:symlink)
		   (when (not (third item))
		     (error "bad map: symlink ~a has no target."
			    (second item)))
		   (make-mapping-entry :kind :symlink
				       :id (second item)
				       :path (third item)
				       :target (fourth item)
				       :raw-plist (fifth item)))
		(otherwise (error "bad type keyword ~s in map." 
				  (first item)))))
	  raw-filemap))

(defun mapping-convert-out (filemap)
"Converts the internal representation of a Meta-CVS mapping to
the external form that is written out to files."
  (mapcar #'(lambda (entry)
	      (with-slots (kind id path target executable raw-plist) entry
		(if executable
		  (setf (getf raw-plist :exec) t)
		  (remf raw-plist :exec))
		(case kind
		  ((:file) (list* kind id path 
				  (if raw-plist (list raw-plist))))
		  ((:symlink) (list* kind id path target
				     (if raw-plist (list raw-plist))))
		  (otherwise (error "unknown mapping entry type ~s." kind)))))
	  filemap))

(defun mapping-read-raw-map (stream)
  (let ((*read-eval* nil))
    (let ((map (read stream nil :error)))
      (if (or (eq map :error) 
		(and (not (consp map)) (not (null map))))
	(malformed-map)
	map))))

(defun mapping-read (source &key sanity-check)
"Reads a Meta-CVS from a file, optionally performing a check
for duplicate entries"
  (let (filemap)
    ;;
    ;; Read the raw data, ensure that the file contains
    ;; a Lisp object and that it's a list, or at least a cons.
    ;;
    (if (streamp source)
      (setf filemap (mapping-read-raw-map source))
      (restart-case
	(with-open-file (stream source :direction :input)
	  (setf filemap (mapping-read-raw-map stream)))
	(continue ()
	  :report "Pretend that an empty map was correctly read."
	  (setf filemap nil))))
    ;;
    ;; Distinguish between the old-style Meta-CVS map and
    ;; the new one. The old one is a list of lists of strings.
    ;; The new one is a list of lists having a keyword in
    ;; the first position.
    ;;
    (setf filemap (if (or (null filemap) (keywordp (first (first filemap))))
		    (mapping-convert-in filemap)
		    (mapping-convert-old-style-in filemap)))

    (if sanity-check  
      (mapping-dupe-check filemap)
      filemap)))

(defun mapping-write (filemap filename &key sort-map)
  (when *dry-run-option*
    (chatter-debug "not writing to ~a because of -n global option.~%" filename)
    (return-from mapping-write))
  (when sort-map
    (setf filemap (sort (copy-list filemap) 
			#'string< :key #'mapping-entry-id)))
  (let ((raw-filemap (mapping-convert-out filemap)))
    (handler-case 
      (with-open-file (file filename :direction :output :if-exists :supersede)
	(let ((*print-right-margin* 1))
	  (prin1 raw-filemap file)
	  (terpri file)))
      (error (cond) (error "unable to write mapping file: ~a" cond)))))

(defun mapping-synchronize (&key filemap (direction :either))
"Synchronizes the contents of files in the sandbox, and their corresponding
CVS files in the Meta-CVS directory. This must be done before any CVS operation
such as commit or update, so that the Meta-CVS files have the correct contents
reflecting local changes. It must also be done after any CVS update operation, 
to ensure that the newly incorporated changes are propagated to the sandbox"
  (let ((filemap (or filemap (mapping-read *mcvs-map-local*))))
    (dolist (entry filemap)
      (can-restart-here ("Continue synchronizing files.")
	(with-slots (kind id path target executable) entry
	  (when (real-path-exists path)
	    (case kind
	      ((:file)
		 (let ((left id) (right (abstract-to-real-path path)))
		   (case (synchronize-files left right executable
					    :direction direction)
		     ((:left)
		       (chatter-info "sync ~a -> ~a~%" left right))
		     ((:right) 
		       (chatter-info "sync ~a <- ~a~%" left right))
		     ((:same :no-sync))
		     ((:dir)
		       (error "cannot sync, either ~a or ~a is a directory."
			      left right))
		     ((nil) 
		       (error "cannot sync, neither ~a nor ~a exists." 
			      left right)))))
	      ((:symlink)
		 (let* ((symlink (abstract-to-real-path path))
			(linkdata (no-existence-error (readlink symlink))))
		   (when (or (not linkdata)
			     (not (string= linkdata target)))
		     (chatter-info "linking: ~a -> ~a~%" 
				   symlink target)
		     (honor-dry-run (target symlink)
		       (no-existence-error (unlink symlink))
		       (ensure-directories-exist symlink)
		       (symlink target symlink))))))))))))

(defun mapping-difference (old-mapping new-mapping)
"Compute the difference between two mappings. Returns three values:
- a mapping containing only elements added by new-mapping;
- a mapping containing only elements removed by new-mapping; and
- a list of moved items, which contains pairs of elements from both, whose
  object name matches, but path differs."
  (multiple-value-bind (moved-pairs added-items removed-items)
		       (intersection-difference 
			 new-mapping old-mapping
			 :key #'mapping-entry-id :test #'equal
			 :combine #'(lambda (new old)
				      (unless (string= (mapping-entry-path new) 
						       (mapping-entry-path old))
					(list old new)))
			 :squash-nil t)
    (values added-items removed-items moved-pairs)))

(defun mapping-update (&key no-delete-removed)
#.(format nil
"Reads the Meta-CVS mapping files ~a and ~a, the local 
mapping and repository mapping, respectively. It computes the difference
between them and then reorganizes the file structure of the sandbox as
necessary to make the mapping up to date. Then the local mapping file is
overwritten so it is identical to the repository one.  This is necessary to
bring the local structure up to date after incorporating mapping changes
whether they came from the CVS repository, or from local operations."
*mcvs-map-local* *mcvs-map*)
  (let ((old-filemap (mapping-read *mcvs-map-local*))
	(new-filemap (mapping-read *mcvs-map* :sanity-check t))
	(rollback-needed t)
	rollback-remove-items rollback-restore-items)
    (unwind-protect
      (multiple-value-bind (added-items removed-items moved-pairs)
			   (mapping-difference old-filemap new-filemap)
	;; First remove what has to be removed. This way when we
	;; do sanity checks, we won't complain about clobbering things
	;; that are slated to disappear.
	(dolist (item removed-items)
	  (when (real-path-exists (mapping-entry-path item))
	    (let ((real (abstract-to-real-path (mapping-entry-path item))))
	      (chatter-terse "removing ~a~%" real)
	      (unless no-delete-removed
		(restart-case 
		  (honor-dry-run (real)
		    (ensure-directories-gone real))
		  (continue () :report "Ignore file removal error."))
		(push item rollback-restore-items)))))

	(dolist (pair moved-pairs)
	  (let ((old-item (first pair)))
	    (with-slots (path) old-item
	      (when (real-path-exists path)
		(honor-dry-run (path)
		  (ensure-directories-gone (abstract-to-real-path path)))
		(push old-item rollback-restore-items)))))
	
	;; Now check sanity of adds and moves, to verify they don't
	;; clobber any local files.
	(let (clobber-add-items clobber-move-pairs)
	  (dolist (item added-items)
	    (with-slots (kind id path target) item
	      (when (real-path-exists path)
		(let* ((real-path (abstract-to-real-path path))
		       (file-info (exists real-path)))
		  (when (and file-info
			     (case kind
			       ((:file) 
				  (not (same-file-p file-info (stat id))))
			       ((:symlink) 
				  (not (string= (readlink real-path)
						target)))
			       (otherwise t))
			     (not (mapping-lookup old-filemap path)))
		    (push item clobber-add-items))))))
	  
	  (dolist (item moved-pairs)
	    (destructuring-bind (old-item new-item) item
	      (declare (ignore old-item))
	      (with-slots (path) new-item
		(when (real-path-exists path)
		  (let ((file-info (exists (abstract-to-real-path path))))
		    (when (and file-info
			       (not (mapping-lookup old-filemap path)))
		      (push item clobber-move-pairs)))))))

	  (when (or clobber-add-items clobber-move-pairs)
	    (super-restart-case
	      (error "some moves or adds want to overwrite local files or directories.")
	      (info ()
		:report "Print list of adds or moves which want to overwrite." 
		(dolist (item clobber-add-items)
		  (format t "add: ~a~%" 
			  (abstract-to-real-path (mapping-entry-path item))))
		(dolist (pair clobber-move-pairs)
		  (format t "move ~a -> ~a~%" 
			  (abstract-to-real-path (mapping-entry-path
						   (first pair)))
			  (abstract-to-real-path (mapping-entry-path
						   (second pair))))))
	      (continue ()
		:report "Go ahead and overwrite the target files." 
		(unwind)))))

	;; Sanity check passed, complete moves and adds.
	(dolist (item moved-pairs)
	  (destructuring-bind (old-item new-item) item
	    (with-slots ((old-path path) (old-id id)) old-item
	      (with-slots ((new-path path) (new-id id) kind target executable) 
			  new-item
		(let ((real-old-exists (real-path-exists old-path))
		      (real-new-exists (real-path-exists new-path)))
		  (let ((real-old (and real-old-exists
				       (abstract-to-real-path old-path)))
			(real-new (and real-new-exists
				       (abstract-to-real-path new-path))))
		    (cond
		      ((and real-old-exists real-new-exists)
			 (chatter-terse "moving ~a -> ~a~%" real-old real-new))
		      (real-new-exists
			 (chatter-terse "moving (out-of-sandbox) -> ~a~%" real-new))
		      (real-old-exists
			 (chatter-terse "moving ~a -> (out-of-sandbox)~%" real-old)))
		      
		    (when real-new-exists
		      (no-existence-error (honor-dry-run (real-new)
					    (unlink real-new)))
		      (case kind
			((:file) 
			   (synchronize-files new-id real-new executable 
					      :direction :right))
			((:symlink) 
			   (honor-dry-run (target real-new)
			     (ensure-directories-exist real-new)
			     (symlink target real-new))))
		      (push new-item rollback-remove-items))))))))

	(dolist (item added-items)
	  (with-slots (kind id path target executable) item
	    (when (real-path-exists path)
	      (let ((real (abstract-to-real-path path)))
		(can-restart-here ("Continue updating file structure.")
		  (no-existence-error (honor-dry-run (real)
					(unlink real)))
		  (case kind
		    ((:file) 
		       (chatter-terse "adding ~a~%" real)
		       (synchronize-files id real executable
					  :direction :right))
		    ((:symlink)
		       (chatter-terse "linking ~a -> ~a~%" real target)
		       (honor-dry-run (real)
			 (ensure-directories-exist real)
			 (symlink target real))))
		  (push item rollback-remove-items))))))
	(setf rollback-needed nil))
      (when rollback-needed
	(chatter-debug "Undoing directory structure changes.~%")
	(dolist (item rollback-remove-items)
	  (let ((real (abstract-to-real-path (mapping-entry-path item))))
	    (chatter-terse "removing ~a~%" real)
	    (honor-dry-run (real)
	      (ensure-directories-gone real))))
	(dolist (item rollback-restore-items)
	  (with-slots (kind path id target executable) item
	    (let ((real (abstract-to-real-path path)))
	      (chatter-terse "restoring ~a~%" real)
	      (case kind
		((:file) 
		   (synchronize-files id real executable :direction :right))
		((:symlink) 
		   (honor-dry-run (real)
		     (ensure-directories-exist real)
		     (symlink target real)))))))))
    (mapping-write new-filemap *mcvs-map-local*))
  t)

(defun mapping-removed-files (filemap)
  (let ((to-be-removed ())
	(f-hash (make-hash-table :test #'equal)))
    (dolist (entry filemap)
      (setf (gethash (mapping-entry-id entry) f-hash) entry))
    (for-each-file-info (fi *mcvs-dir*)
      (when (and (directory-p fi)
		 (path-equal (basename (file-name fi)) "CVS"))
	(skip))
      (let ((base (basename (file-name fi))))
	(multiple-value-bind (suffix name) (suffix base)
	  (declare (ignore suffix))
	  (when (and (= (length name) 34)
		     (string= (subseq name 0 2) "F-")
		     (not (gethash (file-name fi) f-hash)))
	    (push (file-name fi) to-be-removed)))))
    to-be-removed))

(defun displaced-path-read ()
  (let ((*read-eval* nil))
    (ignore-errors (with-open-file (file *mcvs-displaced* :direction :input)
		     (read file)))))

(defun displaced-path-write (path)
  (with-open-file (file *mcvs-displaced* :direction :output)
    (prin1 path file)
    (terpri file)))
