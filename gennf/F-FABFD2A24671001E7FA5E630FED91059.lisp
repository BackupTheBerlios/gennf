;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(provide "multi-hash")

(defclass multi-hash ()
  ((dimensions :initarg :dimensions :accessor dimensions)
   (root-hash :initform nil)
   (tests :initform nil :initarg :tests :accessor tests)))

(defmethod initialize-instance :after ((h multi-hash) &rest stuff)
  (declare (ignore stuff))
  (with-slots (dimensions root-hash tests) h
    (setf root-hash (make-hash-table :test (or (first tests) #'eql)))))

(defmacro multi-hash-common-code (setf-p)
  `(with-slots (dimensions root-hash tests) multi-hash
     (do* ((i 0 (1+ i))
	   (next-hash nil (or (gethash (first arg) current-hash)
			      ,(if setf-p
				 `(setf (gethash (first arg) current-hash)
					(make-hash-table :test (or (nth i tests) 
								 #'eql)))
				 `(return (values nil nil)))))
	   (arg args (rest arg))
	   (current-hash root-hash next-hash))
	  ((= i (1- dimensions)) 
	    ,(if setf-p
	       `(setf (gethash (first arg) current-hash) (second arg))
	       `(gethash (first arg) current-hash))))))

(defun get-multi-hash (multi-hash &rest args)
  (multi-hash-common-code nil))

(defun set-multi-hash (multi-hash &rest args)
  (multi-hash-common-code t))

(defsetf get-multi-hash set-multi-hash)
