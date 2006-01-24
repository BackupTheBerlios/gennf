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
;; $Id: F-48D1C070D93800E7560AEE00EF78D0B2.lisp,v 1.3 2006/01/24 17:38:07 sigsegv Exp $

(in-package :gennf)

(defmacro extract (symbol symbol-alist)
  `(cdr (assoc ,symbol ,symbol-alist)))

(defun unassoc (symbol symbol-alist)
  (remove-if (lambda (pair) (eql symbol (car pair))) symbol-alist))

(defun reassoc (symbol data symbol-alist)
  (acons symbol data (unassoc symbol symbol-alist)))

(defmacro with-gensyms ((&rest names) &body forms)
  "Taken from Peter Seibel's book, chapter 8."
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@forms))

(defmacro ensure-string-pathname (pathspec)
  `(when (typep ,pathspec 'pathname)
    (setf ,pathspec (namestring ,pathspec))))

(defun search-multiple (sequences sequence)
  (let ((results ()))
    (dolist (item sequences)
      (when (search item sequence)
	(push item results)))
    results))

(defun pathname-prefixes (pathspec)
  "Given a pathspec /dir1/dir2/dir3/file a list of
all directory-prefixes is generated:
/, /dir1/, /dir1/dir2/, /dir1/dir2/dir3/."
  (port-path:with-pathname ((pathname pathspec))
    (let ((directory-prefixes
	   ;; Generate alle prefixes in reverse order. The longest
           ;; prefix is first and all directories appear in reverse
	   ;; order, too.
	   (maplist #'identity (reverse (pathname-directory pathname))))
	  pathname-prefixes)
      (dolist (prefix directory-prefixes)
	;; By using push, the shortest prefix is first in list and
	;; by reversing prefix, directories are put in correct order.
	(push (make-pathname :directory (reverse prefix)
			     :name nil :type nil
			     :defaults pathname) pathname-prefixes))
      pathname-prefixes)))


;; Does this macro leak?
;; More conditions could be supported.
(defmacro retry-until-finished ((condition cleanup) &body body) 
  (let  ((retry (gensym))
	 (condi (gensym)))
    `(loop with ,retry do
      (setf ,retry nil)
      (handler-case (progn ,@body)
	(,condition (,condi) (funcall ,cleanup ,condi)
		       (setf ,retry t)))
     while ,retry)))
