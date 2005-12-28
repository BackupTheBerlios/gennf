;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

(defmacro with-slot-refs ((&rest slot-entries) instance-form &body forms)
"A macro similar to with-slots, except that each occurence of any
of the bound variables newly evaluates instance-form."
  (let ((slot-macrolets
	  (mapcar #'(lambda (e)
		      (cond
			((consp e)
			   (when (or (not (= (length e) 2))
				     (not (symbolp (first e)))
				     (not (symbolp (second e))))
				 (error "with-slots-*: slot entry ~a must be two symbols." e))
			   `(,(first e) (slot-value ,instance-form ',(second e))))
			((symbolp e)
			   `(,e (slot-value ,instance-form ',e)))
			(t (error "with-slots-*: slot entry ~a must be a symbol." e))))
		  slot-entries)))
   `(symbol-macrolet ,slot-macrolets ,@forms)))

(defmacro with-multi-slot-refs ((&rest refs) &body forms)
"Allows nested slot-shorthand invocations to be collapsed. That is:
  (with-slot-refs (E-1) I-1 ... ( ...  (with-slot-refs (E-N) I-N F) ... ) ...)
can be rewritten:
  (with-slot-refs-* ((E-1) I1 ... (E-N) V-N) F)"
 (let (refs-pairs (expansion forms))
   (do ((entries (pop refs) (pop refs))
	(instance (pop refs) (pop refs)))
       ((null entries))
       (push (list entries instance) refs-pairs))
   (if (null refs-pairs)
    `(progn ,@expansion)
     (dolist (refs-pair refs-pairs (first expansion))
       (setf expansion `((with-slot-refs ,@refs-pair ,@expansion)))))))

