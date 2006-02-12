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
;; $Id: F-495FEBB82844E29E9B8F3FFCC5A022E2.lisp,v 1.3 2006/02/12 22:15:07 florenz Exp $

;; This file contains routines to manipulate access entries.

(in-package :gennf)

(defclass access ()
  ((identifier :initarg :identifier
	       :initform 1
	       :accessor identifier
	       :documentation "A unique natural number.")
   (backend :initarg :backend
	    :accessor backend
	    :initform :cvs
	    :documentation "Which backend to use.")
   (root :initarg :root
	 :accessor root
	 :documentation "The root string (for cvs)."))
  (:documentation "An access describes how to reach
a certain repository."))

(defun is-access-p (access)
  (and (= (length access) 3)
       (assoc :identifier access)
       (assoc :backend access)
       (assoc :root access)))

(defmethod convert-to-alist append ((access access))
  "Convert an access object to an alist."
  (acons :identifier (identifier access)
	 (acons :backend (backend access)
		(acons :root (root access) ()))))

(defun alist-to-access (alist)
  "Converts a proper alist to an access object."
  (when (is-access-p alist)
    (make-instance 'access
		   :identifier (extract :identifier alist)
		   :backend (extract :backend alist)
		   :root (extract :root alist))))

(defmethod print-object ((access access) stream)
  "Prints an access as an alist, only if *print-readably* is NIL."
  (if *print-readably*
      (call-next-method)
      (prin1 (convert-to-alist access) stream)))

(defun read-access-file (&optional (file *access-file*))
  "Reads an access file and returns a sequence of
accesss objects."
  (mapcar #'alist-to-access (read-file file)))

(defun write-access-file (sequence &optional (file *access-file*))
  "Writes the sequence of access objects to file."
  (prin1-file file sequence))

(defgeneric get-new-access-identifier (store)
  (:documentation "Get a new identifier for an access by returning the
next free number."))

(defmethod get-new-access-identifier ((file pathname))
  "Computes a new access identifier using the access sequence in file."
  (get-new-access-identifier (read-file file)))

(defmethod get-new-access-identifier ((sequence list))
  "The new access identifier is length of sequence plus one."
  (1+ (length sequence)))

(defgeneric add-access (access store)
  (:documentation "Add a new access to store."))

(defmethod add-access (access (file pathname))
  "Put the new access into file."
  (prepend-to-list-file file access))

(defmethod add-access (access (sequence list))
  "Return a new sequence of accesses with new one
being head."
  (cons access sequence))

(defgeneric get-access (identifier store)
  (:documentation "Return the indicated access from store."))

(defmethod get-access (identifier (file pathname))
  "Read the requested access from file."
  (get-access identifier (read-file file)))

(defmethod get-access (identifier (sequence list))
  "Return the requested idenitifer from sequence."
  (nth identifier sequence))