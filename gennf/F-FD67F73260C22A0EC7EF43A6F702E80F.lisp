;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "memoize")
;(provide "seqfuncs")

(defun separate-if (test sequence &rest keys)
  (let ((wheat (apply #'remove-if-not test sequence keys))
        (chaff (apply #'remove-if test sequence keys)))
    (values wheat chaff)))

(defun separate (item sequence &key (test #'eql) key)
  (let ((wheat (funcall #'remove-if-not #'(lambda (x) (funcall test item x))
				      sequence :key key))
        (chaff (funcall #'remove item sequence :key key :test test)))
    (values wheat chaff)))

(define-memoized-function lcs-list ((list-1 :test #'eq)
				    (list-2 :test #'eq) 
				    &key (test #'eql))
  (cond
    ((null list-1) nil)
    ((null list-2) nil)
    ((funcall test (first list-1) (first list-2))
       (cons (first list-1) (lcs-list (rest list-1) (rest list-2))))
    (t (let ((lcs-1 (lcs-list list-1 (rest list-2)))
	     (lcs-2 (lcs-list (rest list-1) list-2)))
	 (if (> (length lcs-1) (length lcs-2))
	   lcs-1
	   lcs-2)))))

(defun lcs-vector (vec-1 vec-2 &key (test #'eql))
  (let ((list-1 (coerce vec-1 'list))
	(list-2 (coerce vec-2 'list)))
    (coerce (lcs-list list-1 list-2 :test test) 'vector)))

(defun longest-common-subsequence (seq-1 seq-2 &key (test #'eql))
  (etypecase seq-1
    (list (lcs-list seq-1 seq-2 :test test))
    (vector (lcs-vector seq-1 seq-2 :test test))))

(defun intersection-difference (seq1 seq2 
				&key (key #'values) key1 key2 (test #'eql)
				(combine #'values) squash-nil)
"Finds the intersection, and mutual differences between two sets.
Returns three values: a sequence of elements that are members of seq1 and seq2;
a sequence of elements that are in seq1 only; and a sequence of elements
that are in seq2 only.

Arguments and values:

seq1 seq2       The input sequences.
:key            Monadic function that specifies what part of
                the element of either sequence to extract. By default,
                takes the element itself as the value.
:key1           Override :key value for elements of seq1.
:key2           Override :key value for elements of seq2.
:combine        Dyadic function which specifies how matching elements from seq1 and
                seq2 are combined to form the intersection. The parameters
                to the function are an element from seq1 and a matching
                counterpart from seq2. The default function takes 
                the seq1 element.
:squash-nil     If the combine function returns NIL, do not include the
                value in the intersection set. Default is NIL, do not squash."
  (setf key1 (or key1 key))
  (setf key2 (or key2 key))

  (let ((hash1 (make-hash-table :test test))
	(hash2 (make-hash-table :test test))
	(intersection ())
	(difference1 ())
	(difference2 ()))
    (dolist (i1 seq1)
      (setf (gethash (funcall key1 i1) hash1) i1))
    (dolist (i2 seq2)
      (setf (gethash (funcall key2 i2) hash2) i2)
      (multiple-value-bind (i1 found)
			   (gethash (funcall key2 i2) hash1)
	(if found
	  (let ((combined (funcall combine i1 i2)))
	    (unless (and squash-nil (null combined))
	      (push (funcall combine i1 i2) intersection)))
	  (push i2 difference2))))
    (dolist (i1 seq1)
      (multiple-value-bind (i2 found)
			   (gethash (funcall key1 i1) hash2)
	(declare (ignore i2))
	(unless found
	  (push i1 difference1))))
    (values intersection difference1 difference2)))
