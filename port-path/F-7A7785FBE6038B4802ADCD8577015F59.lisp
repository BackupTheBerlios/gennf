;; Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
;;
;; This file is part of port-path.
;;
;; port-path is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; port-path is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gennf; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; $Id: F-7A7785FBE6038B4802ADCD8577015F59.lisp,v 1.1 2005/12/27 16:06:27 florenz Exp $


(in-package :port-path)


(defmacro with-pathname (variable-pathspec-pairs &body forms)
  "macro WITH-PATHNAME variable-pathspec-pairs &body forms

Assigns each variable the pathname denoted by a pathspec (may
be a pathname, open or closed stream or string)."
  `(let ,(mapcar
	  (lambda (pair)
	    `(,(first pair) (pathname ,(second pair))))
	  variable-pathspec-pairs)
    ,@forms))


(defmacro with-directory-form (variable-pathspec-pairs &body forms)
  "macro WITH-DIRECTORY-FORM variable-pathspec-pairs &body forms

Assigns each variable the pathname in directory form denoted
by pathspec."
  `(let ,(mapcar
	  (lambda (pair)
	    `(,(first pair) (pathname-to-directory-form ,(second pair))))
	  variable-pathspec-pairs)
    ,@forms))


(defun component-defined-p (component)
  "function COMPONENT-DEFINED-P component => generalized-boolean

Test if a given component of a pathname is defined, i. e.
non-nil or unspecific."
  (and component (not (eql component :unspecific))))


(defun directory-pathname-p (pathspec)
  "function DIRECTORY-PATHNAME-P pathspec => generalized-boolean

Test if a given pathname is in directory form (i. e. its
:name and :type components are not defined)."
  (and
   (not (component-defined-p (pathname-name pathspec)))
   (not (component-defined-p (pathname-type pathspec)))
   pathspec))


(defun pathname-to-directory-form (pathspec)
  "function PATHNAME-TO-DIRECTORY-FORM pathspec => pathname

If pathspec is not in directory form its :name and
:type component are appended to its :directory component,
i. e. it is converted into directory-form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Cannot convert pathnames with wildcards into directory form."))
    (if (not (directory-pathname-p pathname))
      (make-pathname
       :directory (append
		   (or (pathname-directory pathname) '(:relative))
		   `(,(file-namestring pathname)))
       :name nil
       :type nil
       :defaults pathname)
      pathname)))


; I am not sure about this method:
; CLISP, GCL, SBCL and CMUCL have either :absolute
; or :root as the only element in the directory list.
; But I do not know if this can be relied on for other
; implementations.
(defun root-p (pathspec)
  "function ROOT-P pathspec => generalized boolean

Checks if pathspec is the file system's root."
  (and
   (eql nil (pathname-name pathspec))
   (eql nil (pathname-type pathspec))
   (= 1 (length (pathname-directory pathspec)))))


(defun get-parent-directory (pathspec)
  "fuction GET-PARENT-DIRECTORY pathspec => pathname

Returns the parent directory of pathspec. The parent of
the root directory is the root itself."
  (if (root-p pathspec)
      pathspec
      (if (directory-pathname-p pathspec)
	  (make-pathname :defaults pathspec
			 :directory (butlast (pathname-directory pathspec)))
	  (make-pathname :defaults pathspec
			 :name nil
			 :type nil))))