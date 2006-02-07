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
;; $Id: F-83B842F2203F2AE6157E430B77F56938.lisp,v 1.2 2006/02/07 18:05:08 florenz Exp $

;; This file provides functions to manipulate meta files
;; that contain sequences of alists.

(in-package :gennf)

(defun read-file (file)
  (with-open-file (stream file :direction :input)
    (read stream)))

(defun prin1-file (file content)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (let ((*print-readably* t)) (prin1 content stream))))

(defun prepend-to-list-file (file &optional element)
  (let (content)
    (if (port-path:path-exists-p file)
	(progn
	  (setf content (read-file file))
	  (when element (push element content)))
	(setf content (if element (list element) ())))
    (prin1-file file content)))