;; Copyright 2006 Hannes Mehnert, Florian Lorenzen, Fabian Otto
;;
;; This file is part of gennf.
;;
;; gennf is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; gennf is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gennf; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; $Id: F-03018E8471DDD49AD47174CF158A9286.lisp,v 1.1 2006/01/16 07:47:42 florenz Exp $

(in-package :gennf)

(defun read-change-file (&optional (file *change-file*))
  (read-file file))

(defun write-change-file (changes &optional (file *change-file*))
  (prin1-file file changes))

(defun create-new-change-file (&optional (file *change-file*))
  (write-change-file () file))

(defun create-new-file-map ()
  ())

(defun lookup-in-file-map (file file-map)
  (cdr (assoc file file-map :test #'string=)))

(defun add-to-file-map (file occurence file-map)
  (if (lookup-in-file-map file file-map)
      file-map
      (acons file occurence file-map)))

(defun add-file-to-change (file occurence change)
  (exchange-file-map change
		     (add-to-file-map file occurence
				      (extract :file-map change))))

(defun exchange-file-map (change file-map)
  (reassoc :file-map file-map change))

(defgeneric add-file-to-changes (file store)
  (:documentation "Add a mapping for file in the latest change of the sequence,
i. e. to the head of the sequence"))

(defmethod add-file-to-changes (file (sequence list))
  (when (typep file 'pathname)
    (setf file (namestring file)))
  (let ((occurences 0)
	(head (car sequence))
	(tail (cdr sequence)))
    (if (lookup-in-file-map file (extract :file-map head))
	sequence
	(progn
	  (dolist (change sequence)
	    (let ((file-map (extract :file-map change)))
	      (when (lookup-in-file-map file file-map)
		(incf occurences))))
	  (cons (add-file-to-change file (1+ occurences) head) tail)))))

(defmethod add-file-to-changes (file (change-file pathname))
  (write-change-file (add-file-to-changes file (read-file change-file))))

(defgeneric get-new-change-identifier (store))

(defmethod get-new-change-identifier ((file pathname))
  (get-new-change-identifier (read-file file)))

(defmethod get-new-change-identifier ((sequence list))
  (1+ (length sequence)))
  
(defgeneric get-change (identifier store))

(defmethod get-change (identifier (sequence list))
  (nth identifier sequence))

(defmethod get-change (identifier (file pathname))
  (get-change identifier (read-file file)))

(defgeneric add-change (change store))

(defmethod add-change (change (sequence list))
  (cons change sequence))

(defmethod add-change (change (file pathname))
  (prepend-to-list-file file change))