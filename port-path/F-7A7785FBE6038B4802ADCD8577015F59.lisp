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
;; $Id: F-7A7785FBE6038B4802ADCD8577015F59.lisp,v 1.4 2006/01/25 19:51:15 florenz Exp $


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


(defun pathname-to-file-form (pathspec)
  "function PATHNAME-TO-FILE-FORM pathspec => pathname

If pathspec is in directory-form it is converted to
file-form, i. e. the last directory-component is
put into the name- and type-components."
  (with-pathname ((pathname pathspec))
    (when (wild-pathname-p pathspec)
      (error "Cannot convert pathnames with wildcards into file-form."))
    (if (directory-pathname-p pathspec)
	(let*
	    ((directories (pathname-directory pathspec))
	     (file-name (pathname (first (last directories)))))
	  (make-pathname
	   :directory (butlast directories)
	   :name (pathname-name file-name)
	   :type (pathname-type file-name)))
	pathspec)))


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


(defun directory-wildcard (directory)
  "function DIRECTORY-WILDCARD directory => pathname

Returns a pathname with wildcards in the file name
element suitable to produce a directory listing
with DIRECTORY."
  (with-directory-form ((pathname directory))
    (make-pathname
     :name :wild
     :type #-clisp :wild #+ clisp :nil
     :defaults pathname)))


(defun directory-listing (directory)
  "function DIRECTORY-LISTING directory => list

Returns the directory listing as a pathname list including both
directories and files."
  (with-directory-form ((pathname directory))
    (when (wild-pathname-p pathname)
      (error "Can not list content of a wildcard path."))
    (let ((directory-wildcard (directory-wildcard pathname)))
      #+(or sbcl cmu lispworks)
      (directory directory-wildcard)
      #+openmcl
      (directory wildcard :directories t)
      #+allegro
      (directory wildcard :directories-are-files t)
      #+clisp
      (nconc
       (directory wildcard)
       (directory (subdirectory-wildcard wildcard)))
      #-(or sbcl cmu lispworks openmcl allegro clisp)
      (error "DIRECTORY-LISTING only available for SBCL, CMUCL, LispWorks, OpenMCL, Allegro, CLISP"))))


#+clisp
(defun subdirectory-wildcard (wildcard)
  "function SUBDIRECTORY-WILDCARD wildcard => pathname

Returns a pathname to produce a directory listings with
all subdirectories of wildcard with CLISP."
  (make-pathname
   :name nil
   :type nil
   :directory (append (pathname-directory wildcard) (list :wild))
   :defaults wildcard))


(defun path-exists-p (pathspec)
  "function PATH-EXISTS-P pathspec => generalized boolean

Tests if the given pathspec exists (be it a file or a directory)
and return a pathname in either directory- or file-form if so."
  (with-pathname ((pathname pathspec))
    #+(or sbcl lispworks openmcl)
    (probe-file pathname)
    #+(or allegro cmucl)
    (or (probe-file (pathname-to-directory-form pathname))
	(probe-file pathname))
    #+clisp
    (or (ignore-errors
	  (probe-file (pathname-to-file-form pathname)))
	(ignore-errors
	  (when (ext:probe-directory (pathname-to-directory-form pathname))
	    pathname)))))

(defun pathname-prefixes (pathspec)
  "Given a pathspec /dir1/dir2/dir3/file a list of
all directory-prefixes is generated:
/, /dir1/, /dir1/dir2/, /dir1/dir2/dir3/."
  (with-pathname ((pathname pathspec))
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