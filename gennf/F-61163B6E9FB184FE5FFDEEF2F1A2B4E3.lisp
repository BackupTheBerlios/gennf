;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(provide "find-bind")

(defmacro vector-bind (vars vec &rest forms)
  (do ((i 0 (1+ i))
       (var vars (rest var))
       (list))
      ((null var) `(let ,(nreverse list) ,@forms))
    (push `(,(first var) (aref ,vec ,i)) list)))

(defun find-bind-extract-vals (key-vec default-vec sequence
			       test-func key-func take-func
			       &key remainder-p)
"This is an internal function that performs the sequence processing 
that underlies the find-bind macro."
  (let ((unique '#:unique)
	(remainder))
    (macrolet ((process-item (item-place remainder-save-form)
		 `(let* ((item (funcall key-func ,item-place))
			 (match (position item key-vec 
					  :test (lambda (x y)
						  (cond
						    ((eq x unique) nil)
						    ((eq y unique) nil)
						    (t (funcall test-func 
								x y)))))))
		    (cond
		      (match
			 (setf (aref default-vec match) 
			       (funcall take-func ,item-place))
			 (setf (aref key-vec match) unique))
		      (t (when remainder-p ,remainder-save-form))))))
      (typecase sequence
	(list
	  (dolist (element sequence)
	    (process-item element (push element remainder)))
	  (setf remainder (nreverse remainder)))
	(vector
	  (setf remainder (make-array (list (length sequence)) :fill-pointer 0))
	  (dotimes (i (length sequence))
	    (process-item (aref sequence i)
			  (vector-push (aref sequence i) remainder))))
	(otherwise
	  (error "FIND-BIND: value ~s specified as SEQUENCE parameter.~%"
		 sequence))))
    (values default-vec remainder)))
    
(defmacro find-bind ((&key (test '#'eql) 
			   (key '#'values) 
			   (take '#'values))
		     (&optional remainder &rest var-specs)
		     sequence &body forms)
"Slick macro for binding variables to elements of a sequence
by matching keys.

Syntax:

  find-bind ({:test test-func} {:key key-func} {:take take-func})
	    ({rem-var} {(var key {default-value})}*) 
            sequence {decl}* {form}* 

Arguments:

  var               A symbol naming a variable to be bound.
  key               A value to be sought after in the input sequence.
  default-value     Optional value to bind to the variable if a match
                    is not found, or the variable would otherwise be
		    bound to the value NIL.
  rem-var           A symbol naming the variable to be bound to
                    a new sequence that has only the unmatched elements
		    from the original sequence.
  test-func         A dyadic comparison function, used to compare
                    elements from the input sequence. Default is #'eql.
  key-func          A monadic function, used to select what part of
                    the sequence elements to compare. Default is to
                    compare the elements themselves.
  take-func         A monadic function, specifies what part of elements
                    to bind to variables (other than rem-var). Default
                    is to take the entire element.
  sequence          The input sequence.

Semantics:

  The specified keys are looked up in the sequence and their corresponding
  variables are bound to the values that are found, or else to their
  default value, or NIL if there is no default value. If NIL happens
  to be the result of an explict match, the default value is substituted,
  if specified.

  The rem-var variable is bound to a new sequence that is stripped of
  these previously located elements. The declarations and forms are
  then evaluated in the lexical environment thus formed.

  The binding occurs left to right. The first occurence of a value
  name in the binding list matches the first occurence of the value
  name in the input sequence. A second occurence of the same value
  matches a second occurence and so on."
  (when (consp remainder)
    (push remainder var-specs)
    (setf remainder nil))
  (let ((vars (mapcar #'first var-specs))
	(keys (mapcar #'second var-specs))
	(defaults (mapcar #'third var-specs))
	(val-sym (gensym "VALS-"))
	(rem-sym (gensym "REM-")))
    `(multiple-value-bind (,val-sym ,rem-sym) 
			  (find-bind-extract-vals (vector ,@keys) 
						  (vector ,@defaults)
						  ,sequence 
						  ,test ,key ,take
						  :remainder-p 
						  ,(not (null remainder)))
       (declare (ignorable ,rem-sym))
       ,(if remainder
	 `(let ((,remainder ,rem-sym)) (vector-bind (,@vars) ,val-sym ,@forms))
	 `(vector-bind (,@vars) ,val-sym ,@forms)))))
