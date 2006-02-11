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
;; $Id: F-080CF3710F46A1C94984A6BD78462AC7.lisp,v 1.4 2006/02/11 21:20:16 florenz Exp $

;; Manipulation of branches and sequences of branches.

(in-package :gennf)

(defclass branch ()
  ((identifier :initarg :identifier
	       :accessor identifier)
   (symbolic-name :initarg :symbolic-name
		  :accessor symbolic-name)
   (description :initarg :description
		:accessor description))
  (:documentation "A branch is a single thread of development.
It is composed of a sequence of changes."))

(defun is-branch-p (alist)
  (and (= (length alist) 3)
       (eql (car (first alist)) :identifier)
       (eql (car (second alist)) :symbolic-name)
       (eql (car (third alist)) :description)))

(defmethod print-object ((branch branch) stream)
  "Prints a branch as an alist, only if *print-readably* is
NIL (see change.lisp for an explaination of this beheviour)."
  (if *print-readably*
      (call-next-method)
      (prin1 (convert-to-alist branch) stream)))

(defmethod convert-to-alist ((branch branch))
  "Convert a branch object to its alist representation."
  (acons :identifier (identifier branch)
	 (acons :symbolic-name (symbolic-name branch)
		(acons :description (description branch) ()))))

(defun alist-to-branch (alist)
  "Converts a proper alist to a branch object."
  (when (is-branch-p alist)
    (make-instance 'branch
		   :identifier (extract :identifier alist)
		   :symbolic-name (extract :symbolic-name alist)
		   :description (extract :description alist))))

(defun read-branch-file ()
  "Reads a branch file and returns a list of branch objects."
  (mapcar #'(lambda (alist) (alist-to-branch alist))
	  (read-file *branch-file*)))

(defun write-branch-file (sequence)
  "Write the sequence of branches to the branch file."
  (prin1-file *branch-file* sequence))
  

(defun create-new-branch-file ()
  "Write a branch file containing an empty list."
  (prin1-file *branch-file* ()))

(defgeneric add-branch (branch store)
  (:documentation "Add a branch object to some store."))

(defmethod add-branch (branch (file pathname))
  "Prepend the branchto the sequence in file."
  (prepend-to-list-file file branch))

(defmethod add-branch (branch (sequence list))
  "Prepend the branch to sequence and return new sequence."
  (cons branch sequence))

(defgeneric get-new-branch-identifier (store)
  (:documentation "Compute a new branch identifier from the branches
already in store."))

(defmethod get-new-branch-identifier ((file pathname))
  "The new branch identifier is the number of branches in
file incremented by one."
  (get-new-branch-identifier (read-branch-file)))

(defmethod get-new-branch-identifier ((sequence list))
  "The identifier returned is length of sequence plus one."
  (1+ (length sequence)))

(defgeneric get-branch (identifier store)
  (:documentation "Extract and return a certain branch object
from store."))

(defmethod get-branch (identifier (file pathname))
  "Return the identified branch read from file."
  (get-branch identifier (read-file file)))

(defmethod get-branch (identifier (sequence list))
  "List position is branch identifier. Return the
indicated item from sequence."
  (nth identifier sequence))

(defun branch-identifier-to-directory (identifier)
  "Returns the relative pathname object associated with
the given identifier."
  (make-pathname :directory (list :relative (format nil "~S" identifier
))))