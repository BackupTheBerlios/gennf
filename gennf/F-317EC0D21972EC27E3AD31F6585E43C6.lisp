;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "multi-hash")
(provide "memoize")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-key-aux-rest (lambda-list)
"Remove any trailing portion of the lambda list that starts with
one of the lambda list keywords &rest, &key, &aux or &body."
    (cond
      ((null lambda-list) nil)
      ((member (first lambda-list) '(&rest &aux &key &body)) nil)
      (t (cons (first lambda-list) (remove-key-aux-rest (rest lambda-list))))))

  (defun strip-lambda-list (list)
"Simplify a lambda list by removing occurences of &optional, stripping
away the trailing portion using REMOVE-KEY-AUX-REST, canonicalizing
simple variable forms to lists so that SYMBOL becomes (SYMBOL), and removing
initializers so that (SYMBOL INITFORM) becomes (SYMBOL). This stripped
lambda list becomes the lambda list of the hidden inner function that
implements the guts of a memoized function, and captures the recursive
calls. The &OPTIONAL parameters are reduced to required ones, and there
are no trailing &KEY or &rest parameters."
    (labels ((simplify-var-form (form)
	       (cond
		 ((eq '&optional form) nil)
		 ((symbolp form) (list form))
		 ((consp form) (list (first form)))
		 (t (error "MEMOIZE-EXPANDER: bad thing in lambda list: ~a~%"
			   form)))))
      (mapcan #'simplify-var-form (remove-key-aux-rest list))))

  (defun extract-tests (lambda-list)
"The memoize module understands special lambda lists which, as an extension
to regular Lisp lambda lists, allow the programmer to specify what test
function should be used for the hash table used to memoize over a given
parameter. This function parses such a lambda list and extracts the tests.
The subtlety here is that after the &optional keyword, the syntax changes.
For required parameters, the syntax which specifies the test is
(SYMBOL :TEST FUNCTION). For &optional paramters, the syntax becomes
(SYMBOL INITFORM :TEST FUNCTION). For any variable which doesn't specify
a test, the test is assumed to be #'eql."
    (let ((saw-optional))
      (mapcan #'(lambda (element)
		  (cond
		    ((eq '&optional element)
		       (setf saw-optional t)
		       nil)
		    ((consp element)
		       (if saw-optional
			 (destructuring-bind (var &optional init &key test) element
			    (declare (ignore var init))
			    (list (or test '#'eql)))
			 (destructuring-bind (var &key test) element
			    (declare (ignore var init))
			    (list (or test '#'eql)))))
		    (t (list '#'eql))))
	      (remove-key-aux-rest lambda-list))))

  (defun remove-tests (lambda-list)
"This function removes the :test specifications from a the memoized
function lambda list, thereby reducing it to a regular lambda list.
See the docstring for EXTRACT-TESTS for a little more information.
We need to do this to generate the outer shell of the memoized function,
which is a normal Lisp function."
    (let (saw-optional saw-key)
      (mapcar #'(lambda (element)
		  (cond
		    ((eq '&optional element)
		       (setf saw-optional t)
		       element)
		    ((eq '&key element)
		       (setf saw-key t)
		       element)
		    ((consp element)
		       (if saw-key
			 element
			 (if saw-optional
			   (destructuring-bind (var &optional init &key test) element
			      (declare (ignore test))
			      (append (list var) (if init (list init))))
			   (destructuring-bind (var &key test) element
			      (declare (ignore test))
			      var))))
		    (t element)))
	      lambda-list)))

(defun memoize-expander (name lambda-list tests body expr)
"Produce a memoized function in the form of a LABELS function that is
wrapped in a LET block. The LET block sets up the hash table, either
a regular hash table if there is one paramter, or a MULTI-HASH if
there are several parameters.  The body of the LABELS performs the 
memoization stuff with the hash tables. Note that the function
FACTOR-MEMO-LABELS depends on this structure of the LABELS nested
within the LET."
  (let ((multi-hash-sym (gensym "MULTI-HASH-"))
	(hash-result-sym (gensym "HASH-RESULT-"))
	(hash-found-sym (gensym "HASH-FOUND-"))
	(dimensions (length lambda-list)))
    `(let ((,multi-hash-sym ,(if (> dimensions 1)
			       `(make-instance 'multi-hash
					       :dimensions ,dimensions
					       :tests ,tests)
			       `(make-hash-table :test ,(second tests)))))
       (labels ((,name ,lambda-list
		  (multiple-value-bind (,hash-result-sym ,hash-found-sym)
				       ,(if (> dimensions 1)
					  `(get-multi-hash ,multi-hash-sym
							   ,@lambda-list)
					  `(gethash ,@lambda-list 
						    ,multi-hash-sym))
		    (if ,hash-found-sym
		      ,hash-result-sym
		      (setf ,(if (> dimensions 1)
			       `(get-multi-hash ,multi-hash-sym ,@lambda-list)
			       `(gethash ,@lambda-list ,multi-hash-sym))
			    (progn ,@body)))))) ,expr))))

(defun factor-memo-labels (memo-labels forms)
"This function takes a list of the LET expressions, each of which is assumed to
be generated by MEMOIZE-EXPANDER, and factors them out to produce one giant
LET block with all of the LET material (hash tables) coalesced together,
enclosing one big coalesced LABELS block that defines all of the functions
together. This trick allows us to generate individual memoized inner functions
using MEMOIZE-EXPANDER, and then fuse them together to make one big
party of mutually recursive memoized functions."
  (macrolet ((destructure-memo-labels (labels &body labels-forms)
	       `(destructuring-bind (name outer-lambda 
				      (let inner-lambda &rest labels-forms)) 
				    ,labels 
		  (declare (ignorable name outer-lambda let inner-lambda
				      labels-forms))
		  ,@labels-forms)))
    (flet ((extract-lets (labels)
	     (destructure-memo-labels labels
	       inner-lambda))
	   (extract-funcs (labels)
	     (destructure-memo-labels labels
	       `(,name ,outer-lambda ,@labels-forms))))
      `(let ,(mapcan #'extract-lets memo-labels)
	 (labels ,(mapcar #'extract-funcs memo-labels) ,@forms))))))

(defmacro define-memoized-function (name lambda-list &body forms)
"Generate a DEFUN definition for a function called NAME, placing the
body into an inner recursive function of the same name that is memoized.
Effectively, this creates a memoized function: one whose recursive calls
are automatically cached using the parameter lists as keys into a multi-level
hash table. This is an important optimization technique when the recursion
contains overlapping cases; it can reduce exponential time to polynomial time.
This macro understands a special lambda list syntax. A required parameter
normally written as SYMBOL can be written (SYMBOL :TEST FUNC) to specify a
hashing equality function FUNC for that parameter which can be #'EQ,
#'EQL, #'EQUAL or #'EQUALP. For an optional parameter, this syntax is
(SYMBOL INIT-FORM :TEST FUNC). Note that only the outer function accepts
&KEY and &REST parameters, if any are specified. The inner recursive memoized
function does not; it has a simplified lambda list."
  (let ((stripped-ll (strip-lambda-list lambda-list)))
    `(defun ,name ,(remove-tests lambda-list)
       ,(memoize-expander name stripped-ll `(list ,@(extract-tests lambda-list))
			  forms `(,name ,@stripped-ll)))))

(defmacro memoized-labels ((&rest memoized-labels-list) &body forms)
"Generate a block of mutually recursive LABELS functions, making the
DEFINE-MEMOIZED-FUNCTION utility available for local functions. See
the documentation string for that macro for more details."
  (flet ((generate-labels-element (labels)
	   (destructuring-bind (name lambda-list &body labels-forms) labels
	     (let ((stripped-ll (strip-lambda-list lambda-list)))
	       `(,name ,(remove-tests lambda-list)
		   ,(memoize-expander name stripped-ll 
				      `(list ,@(extract-tests lambda-list))
				      labels-forms 
				      `(,name ,@stripped-ll)))))))
  (factor-memo-labels (mapcar #'generate-labels-element 
			      memoized-labels-list) forms)))
