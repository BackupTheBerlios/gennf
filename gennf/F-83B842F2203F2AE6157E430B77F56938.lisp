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
;; $Id: F-83B842F2203F2AE6157E430B77F56938.lisp,v 1.4 2006/02/09 13:23:25 sigsegv Exp $

;; This file provides functions to manipulate meta files
;; that contain sequences of alists.

(in-package :gennf)

(defgeneric convert-to-alist (object)
  (:documentation "Generates an alist represenation of object.
The reason, why this generic function is in this file is that
all meta are lists of alists but converted to lists
of objects of different classes internally.
Thus, for each class' objects to be written to a meta
file an appropriate method is to be provided."))

(defun read-file (file)
  "Return a Lisp representation of file's content."
  (with-open-file (stream file :direction :input)
    (read stream)))

(defun prin1-file (file content)
  "Print content readably to file. If file exists, it
is overwritten, if not, it is created."
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (prin1 content stream)
    (terpri stream)))

(defun prepend-to-list-file (file &optional element)
  "Prepend element to the list in file. If file does not
exist, it is created with element as its sole entry."
  (let (content)
    (if (port-path:path-exists-p file)
	(progn
	  (setf content (read-file file))
	  (when element (push element content)))
	(setf content (if element (list element) ())))
    (prin1-file file content)))