;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "dirwalk")
;(require "system")
;(require "split")
;(require "seqfuncs")
;(require "mapping")
;(require "types")
;(require "chatter")
;(require "options")
;(require "checkout")
;(require "remove")
;(require "add")
;(provide "grab")

(defun read-word-hash (&optional (byte-stream t))
  (let ((word-hash (make-hash-table :test #'equalp))
	token
	(state :junk))
    (labels ((new-token () 
	       (if token
		 (setf (fill-pointer token) 0)
		 (setf token (make-array '(8) 
					 :element-type 'character
					 :adjustable t
					 :fill-pointer 0))))
	     (save-token ()
	       (unless (gethash token word-hash)
		 (let ((copy (make-string (length token))))
		   (replace copy token)
		   (setf (gethash copy word-hash) copy)))))
      (do ((byte (read-byte byte-stream nil)
		 (read-byte byte-stream nil)))
	  ((null byte) (if (eq state :word) (save-token)) word-hash)
	(let ((ch (code-char byte)))
	  (ecase state
	    ((:junk)
	      (when (or (alpha-char-p ch) (digit-char-p ch)
			(char= ch #\_))
		(new-token)
		(vector-push-extend ch token)
		(setf state :word)))
	    ((:word)
		(cond
		  ((or (alpha-char-p ch) (digit-char-p ch)
		       (char= ch #\_) (char= ch #\-))
		     (vector-push-extend ch token))
		  (t (setf state :junk)
		     (save-token))))))))))

(defun word-hash-file (name)
  (with-open-file (s (parse-posix-namestring name)
		     :direction :input
		     :element-type 'unsigned-byte)
    (read-word-hash s)))

(defun correlate-word-hashes (hash-1 hash-2)
  (let ((hc-1 (hash-table-count hash-1))
	(hc-2 (hash-table-count hash-2)))
    (when (> hc-1 hc-2)
      (psetf hash-1 hash-2 hash-2 hash-1
	     hc-1 hc-2 hc-2 hc-1))
    (let ((common-count 0))
      (maphash #'(lambda (key element)
		   (declare (ignore key))
		   (when (gethash element hash-2)
		     (incf common-count)))
	       hash-1)
      (let ((total-count (- (+ hc-1 hc-2) common-count)))
	(if (zerop total-count)
	  0
	  (/ common-count total-count))))))

(defun correlate-paths (path-1 path-2)
  (let* ((split-1 (split-fields path-1 *path-sep*))
	 (split-2 (split-fields path-2 *path-sep*))
	 (longer (max (length split-1) (length split-2)))
	 (lcs-len (length (longest-common-subsequence split-1 split-2
						       :test #'string=))))
    (case (- longer lcs-len)
      ((0 1) 1)
      ((2) 95/100)
      ((3) 90/100)
      ((4) 85/100)
      (otherwise 80/100))))

(defun determine-common-words (common-hash added-or-removed-files)
  (dolist (file added-or-removed-files common-hash)
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (let ((existing (gethash key common-hash)))
		   (if existing
		     (setf (gethash key common-hash) 
			   (1+ existing))
		     (setf (gethash key common-hash)
			   1))))
	     (second file))))

(defun eliminate-common-words (common-hash files threshold)
  (dolist (file files)
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (let ((count (gethash key common-hash)))
		   (if (and count (>= count threshold))
		     (remhash key (second file)))))
	     (second file))))
		 
(defun determine-moved-files (added-files removed-files)
  (let (pairs moved-files taken-added taken-removed)
    (dolist (added added-files)
      (dolist (removed removed-files)
	(let ((correlation (correlate-word-hashes (second added) 
						  (second removed))))
	  (when (>= correlation 30/100)
	    (push (list added removed 
			(* correlation (correlate-paths (first added)
							(first removed))))
		  pairs)))))
    (setf pairs (sort pairs #'> :key #'third))
    (dolist (pair pairs)
      (unless (or (member (first pair) taken-added :test #'eq)
		  (member (second pair) taken-removed :test #'eq))
	(push (first pair) taken-added)
	(push (second pair) taken-removed)
	(push pair moved-files)))
    (values moved-files 
	    (set-difference added-files taken-added :test #'eq)
	    (set-difference removed-files taken-removed :test #'eq))))

(defun determine-moved-symlinks (added-symlinks removed-symlinks moved-files
				 stable-files)
  (let ((add-hash (make-hash-table :test #'equal))
	(remove-hash (make-hash-table :test #'equal))
	moved-symlinks taken-added taken-removed)
    (macrolet ((process-item (item path target hash)
		 `(unless (path-absolute-p ,target)
		    (multiple-value-bind (base dir) (basename ,path)
		      (declare (ignore base))
		      (if (null dir)
			(setf dir "."))
		      (multiple-value-bind (resolved-path out-of-bounds)
					   (canonicalize-path (path-cat 
								dir 
								,target))
			(unless out-of-bounds
			  (push ,item (gethash resolved-path ,hash))))))))
      (dolist (added added-symlinks)
	(destructuring-bind (path target) added
	  (process-item added path target add-hash)))
      (dolist (removed removed-symlinks)
	(with-slots (path target) removed
	  (process-item removed path target remove-hash)))
      (macrolet ((move-symlinks (source-name target-name)
		   `(let ((added-list (gethash ,target-name add-hash))
			  (removed-list (gethash ,source-name remove-hash))
			  (symlink-move-pairs))
		      (dolist (added added-list)
			(dolist (removed removed-list)
			  (push (list added removed (correlate-paths 
						      (first added)
						      (mapping-entry-path removed)))
				symlink-move-pairs)))
		      (setf symlink-move-pairs (sort symlink-move-pairs #'> :key #'third))
		      (dolist (pair symlink-move-pairs)
			(unless (or (member (first pair) taken-added :test #'eq)
				    (member (second pair) taken-removed :test #'eq))
			  (push (first pair) taken-added)
			  (push (second pair) taken-removed)
			  (push pair moved-symlinks))))))
	(dolist (file-move-pair moved-files)
	  (destructuring-bind ((target-name hash-2) 
			       (source-name hash-1 f-file) confidence)
			      file-move-pair
	  (declare (ignore hash-1 hash-2 f-file confidence))
	  (move-symlinks source-name target-name)))
	(dolist (entry stable-files)
	  (with-slots (path) entry
	    (move-symlinks path path)))))
    (values moved-symlinks
	    (set-difference added-symlinks taken-added :test #'eq)
	    (set-difference removed-symlinks taken-removed :test #'eq))))

(defun mcvs-grab (global-options command-options module subdir)
  (find-bind (:test #'string= :key #'first)
	     ((branch "r") (trunk "A"))
	     command-options
    (when (and branch trunk)
      (error "both -r and -A specified."))
    (when (and (not branch) (not trunk))
      (error "specify branch using -r or main trunk using -A."))
    (mcvs-checkout module subdir global-options 
		   `(("d" ,*this-dir*) ,@(if branch (list branch)))
		   :no-generate t)
    (in-sandbox-root-dir
      (let ((mapping (mapping-read *mcvs-map*))
	    invisible-old-paths old-paths
	    old-file-paths new-file-paths 
	    old-symlink-paths new-symlink-paths
	    added-files removed-files stable-files
	    added-symlinks removed-symlinks stable-symlinks)
	(chatter-info "Scanning directory structure.~%")
	(multiple-value-setq (old-paths invisible-old-paths)
			     (separate-if #'real-path-exists mapping
					  :key #'mapping-entry-path))
	(dolist (entry old-paths)
	  (with-slots (path) entry
	    (setf path (abstract-to-real-path path))))
	(multiple-value-setq (old-file-paths old-symlink-paths)
			     (separate :file old-paths
				       :test #'eq
				       :key #'mapping-entry-kind))

	(for-each-file-info (fi *this-dir*)
	  (let* ((path (canonicalize-path (file-name fi))))
	    (cond
	      ((regular-p fi)
		 (push path new-file-paths))
	      ((symlink-p fi)
	         (push path new-symlink-paths))
	      ((directory-p fi)
		(when (path-equal path *mcvs-dir*)
		  (skip))))))

	(multiple-value-setq (stable-files removed-files added-files) 
			     (intersection-difference old-file-paths 
						      new-file-paths
						      :key1 #'mapping-entry-path
						      :test #'equal))

	(multiple-value-setq (stable-symlinks removed-symlinks added-symlinks) 
			     (intersection-difference old-symlink-paths 
						      new-symlink-paths
						      :key1 #'mapping-entry-path
						      :test #'equal))


	(cond
	  ((or (null added-files) (null removed-files))
	     (setf added-files (mapcar #'(lambda (name) (list name)) added-files))
	     (setf removed-files (mapcar #'(lambda (entry) 
					     (with-slots (id path) entry
					       (list path nil id)))
					 removed-files)))
	  (t (chatter-terse "Analyzing ~a added file~:p.~%" (length added-files))
	     (setf added-files (mapcar #'(lambda (name) 
					(list name (word-hash-file name)))
				    added-files))

	     (chatter-terse "Analyzing ~a removed file~:p.~%" (length removed-files))
	     (setf removed-files (mapcar #'(lambda (entry)
					     (with-slots (id path) entry
					       (list path
						     (word-hash-file id) id)))
					 removed-files))
	     (let ((common-word-hash (make-hash-table :test #'equalp)))
	       (determine-common-words common-word-hash added-files)
	       (determine-common-words common-word-hash removed-files)
	       (let ((threshold (max 5 (* 1/5 (+ (length added-files)
						 (length removed-files))))))
		 (eliminate-common-words common-word-hash added-files threshold)
		 (eliminate-common-words common-word-hash removed-files threshold)))
	     (chatter-terse "Determining move candidates.~%")))

	(multiple-value-bind (moved-files added-files removed-files)
			     (if (or (null added-files) (null removed-files))
			       (values nil added-files removed-files)
			       (determine-moved-files added-files 
						      removed-files))
	  (when added-symlinks
	    (chatter-terse "Reading ~a added symbolic link~:p.~%" 
			   (length added-symlinks))
	    (setf added-symlinks (mapcar #'(lambda (path)
					     (list path (readlink path)))
					 added-symlinks)))

	  (multiple-value-bind (moved-symlinks added-symlinks removed-symlinks)
			       (if (or (null added-symlinks)
				       (null removed-symlinks))
				 (values nil added-symlinks removed-symlinks)
				 (determine-moved-symlinks added-symlinks 
							   removed-symlinks
							   moved-files
							   stable-files))

	    (let ((moved-hash (make-hash-table :test #'equal))
		  (all-hash (make-hash-table :test #'equal))
		  (mapping (mapping-read *mcvs-map*)))
	      (dolist (entry mapping)
		(setf (gethash (mapping-entry-id entry) all-hash) entry))
	      (when (or moved-files moved-symlinks)
		(dolist (pair moved-files)
		  (destructuring-bind ((target-name hash-2) 
				       (source-name hash-1 f-file) confidence)
				      pair
		    (declare (ignore hash-1 hash-2))
		    (chatter-terse "moving ~a -> ~a (confidence ~a%)~%" 
			    source-name target-name (round (* confidence 100)))
		    (setf (gethash f-file moved-hash) target-name)))
		(dolist (pair moved-symlinks)
		  (destructuring-bind ((target-name symlink-target)
				       source-entry confidence) pair
		    (declare (ignore symlink-target confidence))
		    (with-slots (id (source-name path)) source-entry
		      (chatter-terse "moving symlink ~a -> ~a~%"
				     source-name target-name)
		      (setf (gethash id moved-hash) target-name))))
		    
		(mapc #'(lambda (entry)
			  (with-slots (id path) entry
			    (let ((replacement (gethash id moved-hash)))
			      (if replacement
				(setf path 
				      (real-to-abstract-path replacement))))))
		      mapping)
		(dolist (entry mapping)
		  (with-slots (kind id path executable) entry
		    (when (and (gethash id moved-hash) (eq kind :file))
		      (unlink id)
		      (link (abstract-to-real-path path) id)
		      (setf executable (executable-p id))))))
	      (dolist (symlink-entry stable-symlinks)
		(with-slots (kind id path target) symlink-entry
		  (let ((map-entry (gethash id all-hash)))
		    (setf (mapping-entry-target map-entry)
			  (readlink path)))))
	      (dolist (pair moved-symlinks)
		(with-slots (kind id path target) (second pair)
		  (let ((map-entry (gethash id all-hash)))
		    (setf (mapping-entry-target map-entry)
			  (readlink (abstract-to-real-path 
				      (mapping-entry-path map-entry)))))))
	      (dolist (file-entry stable-files)
		(with-slots (kind id path) file-entry
		  (let ((map-entry (gethash id all-hash)))
		    (setf (mapping-entry-executable map-entry)
			  (executable-p path)))
		  (when (eq kind :file)
		    (unlink id)
		    (link path id))))
	      (mapping-write mapping *mcvs-map*)
	      (mapping-write mapping *mcvs-map-local*))
	    (when removed-files
	      (mcvs-remove nil (mapcar #'first removed-files) :no-sync t))
	    (when removed-symlinks
	      (mcvs-remove nil (mapcar #'mapping-entry-path removed-symlinks) 
			   :no-sync t))
	    (when added-files
	      (mcvs-add nil global-options 
			nil (mapcar #'first added-files)))
	    
	    (when added-symlinks
	      (mcvs-add nil global-options 
			nil (mapcar #'first added-symlinks)))))))))

(defun mcvs-grab-wrapper (global-options command-options args)
  (flet ((fail ()
	   (error "specify module name, and optional subdirectory.")))
    (when (zerop (length args))
      (fail))
    (destructuring-bind (module &optional subdir &rest superfluous) args
      (when superfluous
	(fail))
      (mcvs-grab global-options command-options module subdir))))

(define-constant *grab-help*
"Syntax:

  mcvs grab { -A | -r branch-name } module-name [ subdirectory-path ]

Options:

  -A                Grab to the main trunk.
  -r branch-name    Grab to the specified branch.

Semantics:

  The grab command is a tool for incorporating external code streams
  into a Meta-CVS module.

  Grab works by comparing the contents of the current directory and its
  subdirectories, to the tip of the trunk or a branch of an existing
  Meta-CVS module. It produces a sandbox which contains a minimized set
  of local edits that are needed to make the branch or trunk in the repository
  look exactly like the current directory.

  These local edits have to be committed just like hand-made edits; the grab
  command itself has no effect on the contents of the repository, and does
  not change the local directory in any way other than by creating the MCVS
  subdirectory.

  If it was run with the wrong arguments, the recovery procedure is simply
  to recursively remove the MCVS subdirectory. Then it's possible to run grab
  again with different arguments, as necessary.

  If the subdirectory-path is specified, then grab will operate on 
  just that subdirectory of the module, making just that subtree look
  like the current directory. The result will be a partial sandbox
  containing local edits to just the visible part of the module.
  (See the help for the checkout command, which also takes a subdirectory path
  parameter to create a partial sandbox).

  Either the -A option or the -r option must be specified. This forces
  users to be explicitly clear about where they want the grab to go;
  the main trunk or a branch.

  Grab performs no merging whatsoever. Its job is to place a new document
  baseline at the tip of a code stream. Third party source tracking is
  performed by grabbing snapshots to a branch, and then merging that branch
  in the usual way. ")
