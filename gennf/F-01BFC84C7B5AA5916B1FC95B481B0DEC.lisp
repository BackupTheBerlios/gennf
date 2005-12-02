;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "mapping")
(require "system")
(require "print")
(require "seqfuncs")
(provide "types")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *mcvs-types-name* "TYPES"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *mcvs-types* #.(path-cat *mcvs-dir* *mcvs-types-name*))
  (defconstant *mcvs-new-types* #.(path-cat *mcvs-dir* "TYPES-NEW")))

(defconstant *types-comments*
";;; For each file suffix that appears in the file set, you can specify
;;; the CVS keyword expansion mode, or you can specify that the files having
;;; that suffix should not be imported. This is done by editing the list below.
;;; Here are the symbols you can specify next to each suffix.
;;;
;;;   :default    Expand keyword using default form. (CVS -kkv)
;;;   :name-only  Expand only the keyword name on checkout. (CVS -kk)
;;;   :keep-old   Do not expand keywords, and keep any CVS or RCS keywords
;;;               that are already present in the files. (CVS -ko)
;;;   :binary     Like :keep-old except that the file is treated as
;;;               binary. Not only are keywords not expanded, but line ending
;;;               conversions are not performed either. (CVS -kb)
;;;   :value-only Expand only the keyword value, no dollar signs. (CVS -kv)
;;;   :ignore     Do not import or add these files.
")


(defun types-read (filename)
  (let ((*read-eval* nil))
    (with-open-file (file filename :direction :input)
      (read file))))

(defun types-write (types filename &key comments)
  (when *dry-run-option*
    (chatter-debug "not writing to ~a because of -n global option.~%" 
		   *mcvs-types*)
    (return-from types-write))
  (with-open-file (file filename :direction :output)
    (let ((sorted-types (sort (copy-list types)
			      #'string-lessp :key #'first)))
      (when comments
	(write-string comments file)
	(terpri file))
      (print-assoc-list sorted-types file)
      (terpri file))))

(defun types-sanity-check (types)
  (cond
    ((null types) 
       (values))
    ((consp types)
       (let ((type-spec (first types)))
	 (when (or (not (stringp (first type-spec)))
		   (not (symbolp (second type-spec))))
	   (error "bad syntax in file type treatment specification: ~s" type-spec))
	 (when (not (member (second type-spec)
			    '(:name-only :keep-old :default :value-only
			      :binary :ignore)))
	   (error "unrecognized keyword: ~s" type-spec))) 
       (types-sanity-check (rest types)))
    (t (error "bad syntax in file type treatment specification: ~s" types))))

(defun types-to-import-wrapper-args (types)
  (mapcan #'(lambda (type-spec)
	      (destructuring-bind (suffix treatment) type-spec
		(flet ((gen-option (suf opt)
			 (list "-W" (format nil "*.~a -k '~a'" suf opt))))
		  (ecase treatment
		    ((:name-only) (gen-option suffix "k"))
		    ((:keep-old) (gen-option suffix "o"))
		    ((:binary) (gen-option suffix "b"))
		    ((:value-only) (gen-option suffix "kv"))
		    ((:ignore) nil)
		    ((:default) nil))))) 
	  types))

(defun types-remove-ignores (types mapping)
  (let ((ignores (mapcan #'(lambda (type-spec)
			     (if (eq (second type-spec) :ignore)
			       (list (first type-spec))))
			 types)))
    (remove-if #'(lambda (entry)
		   (with-slots (kind id) entry
		     (and (eq kind :file)
			  (member (suffix id) ignores :test #'path-equal))))
	       mapping)))

(defun types-make-cvs-adds (types mapping)
  (let (cvs-adds matching files)
    (dolist (type-entry types)
      (multiple-value-setq 
	(matching mapping)
	(separate-if #'(lambda (x)
			 (and x (path-equal (first type-entry) x)))
		     mapping
		     :key #'(lambda (x) (suffix (mapping-entry-id x)))))
      (setf files (mapcar #'basename (mapcar #'mapping-entry-id matching)))
      (when files
	(ecase (second type-entry)
	  ((:name-only) (setf files (list* "-kk" files)))
	  ((:keep-old) (setf files (list* "-ko" files)))
	  ((:binary) (setf files (list* "-kb" files)))
	  ((:value-only) (setf files (list* "-kv" files)))
	  ((:ignore) (setf files nil))
	  ((:default)))
	(when files (push files cvs-adds))))
    (setf files (mapcar #'basename (mapcar #'mapping-entry-id mapping)))
    (when files (push files cvs-adds))
    cvs-adds))

(defun types-let-user-edit (types filename)
  (when types
    (types-write types filename :comments *types-comments*)
    (loop 
      (loop
	(restart-case
	  (progn 
	    (chatter-debug "Editing types.~%")
	    (unless (invoke-editor-on filename)
	      (error "Failed to invoke text editor."))
	    (return))
	  (retry ()
	    :report "Try invoking editor again.")))
      (restart-case
	(let ((edited-types (types-read filename)))
	  (types-sanity-check edited-types)
	  (types-write edited-types filename)
	  (return edited-types))
	(retry ()
	  :report "Correct file type treatment, try again.")
	(restore-types ()
	  :report "Revert to original file treatment and edit again."
	  (types-write types filename :comments *types-comments*))))))
