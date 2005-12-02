;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(provide "restart")

(defmacro can-restart-here ((format-string &rest format-args) &body forms)
  (let ((restart-block (gensym "RESTART-"))
        (stream (gensym "STREAM-")))
   `(block ,restart-block
      (restart-bind 
	((continue #'(lambda () (return-from ,restart-block))
	  :report-function #'(lambda (,stream) 
			       (format ,stream ,format-string ,@format-args))))
	,@forms))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-restart-case-keywords (body)
    (let ((interactive-spec ())
	  (test-spec ())
	  (report-spec()))
      (do ((key (first body) (first body))
	   (val (second body) (second body)))
	  ((not (member key '(:report :test :interactive))))
	(pop body)
	(pop body)
	(case key
	  ((:interactive) 
	   (setf interactive-spec `(:interactive-function (function ,val))))
	  ((:report) 
	   (setf report-spec
		 (cond 
		   ((stringp val)
		      `(:report-function #'(lambda (stream) 
					   (write-string ,val stream))))
		   ((and (consp val) (stringp (first val)))
		      `(:report-function #'(lambda (stream) 
					   (format stream ,@val))))
		   (t `(:report-function (function ,val))))))
	  ((:test)
	   (setf test-spec `(:test-function (function ,val))))))
      (values interactive-spec test-spec report-spec body)))

  (defun super-restart-case-expander (clause top-sym out-sym closure-var)
    (destructuring-bind (name lambda-list &rest body) clause
      (multiple-value-bind (interactive-spec test-spec 
			    report-spec body)
			   (parse-restart-case-keywords body)
	`(,name 
	  (macrolet 
	    ((unwind (&body forms)
	       `(progn 
		  (setf ,',closure-var 
			#'(lambda () ,@forms))
		  (go ,',out-sym)))
	     (retry ()
	       `(go ,',top-sym)))
	    #'(lambda ,lambda-list ,@body))
	  ,@interactive-spec ,@report-spec ,@test-spec)))))

(defmacro super-restart-case (expr &rest clauses)
"This is a macro that is very similar to the standard RESTART-CASE, but
with a few refinements. 

Firstly, the special syntax :REPORT (STRING ...) is accepted.
This is a shorthand for 

  :REPORT (LAMBDA (STREAM) (FORMAT STREAM STRING ...)))

Secondly, unlike RESTART-CASE, SUPER-RESTART-CASE does not perform a
non-local exit before executing the body of a clause. A clause body is
executed in the dynamic context of the restart invocation. In other
words, SUPER-RESTART-CASE clause bodies behave like closures specified
to RESTART-BIND. When the last form in the clause body is executed,
control returns back to the context which invoked the restart.

Thirdly, two special local macros may be used in the bodies of the
clauses to achieve control over the behavior. 

The macro (RETRY) will re-execute the entire SUPER-RESTART-CASE form
from the beginning. This provides an easy way to implement retry
behavior, without having to code an explicit loop.

The macro (UNWIND [ FORMS ...]) causes FORMS to be evaluated after
performing a non-local exit back to the dynamic context of the
SUPER-RESTART-CASE form. When the last of the forms is evaluated, the
entire form terminates. As can be expected, the result of the entire
will be the value of the last form in (UNWIND ...)."
  (let ((top-sym (gensym "TOP-"))
	(out-sym (gensym "OUT-"))
	(closure-var (gensym "CLOSURE-")))
    `(let ((,closure-var nil))
       (block nil
	 (tagbody
	   ,top-sym
	   (restart-bind
	     ,(mapcar #'(lambda (clause)
			  (super-restart-case-expander clause top-sym out-sym
						       closure-var))
		      clauses)
	     (return (progn ,expr)))
	   ,out-sym
	   (return (funcall ,closure-var)))))))
