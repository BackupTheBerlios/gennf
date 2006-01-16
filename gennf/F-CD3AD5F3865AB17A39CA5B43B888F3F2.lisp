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
;; $Id: F-CD3AD5F3865AB17A39CA5B43B888F3F2.lisp,v 1.1 2006/01/16 07:47:43 florenz Exp $

(in-package :gennf)

(defmacro in-meta-directory (&body forms)
  (let ((current-directory (gensym "current-directory-")))
    `(let ((,current-directory (current-directory)))
      (change-to-meta-directory)
      ,@forms
      (change-directory ,current-directory))))

(defmacro in-temporary-directory (&body forms)
  `(progn
    ,@forms))

(defun create-meta-directory ()
  (create-directory *meta-directory* :require-fresh-directory t))

(defun change-directory (pathspec)
  (port-path:with-directory-form ((directory (merge-pathnames pathspec)))
    (if (port-path:path-exists-p directory)
	(progn
	  (setf (osicat:current-directory) directory)
	  (setf *default-pathname-defaults* directory))
	(error "Directory ~S does not exist." directory))))

(defun current-directory ()
  *default-pathname-defaults*)

(defun change-directory-up ()
  (change-directory (port-path:get-parent-directory (current-directory))))

(defun change-to-meta-directory ()
  (change-directory *meta-directory*))

(defun remove-meta-directory ()
  (when (port-path:path-exists-p *meta-directory*)
    (change-to-meta-directory)
    (change-directory-up)
    (delete-directory-tree *meta-directory*)))

(defun delete-directory-tree (pathspec)
  (port-path:with-directory-form ((directory pathspec))
    (let ((listing (port-path:directory-listing directory)))
      (dolist (entry listing)
	(if (port-path:directory-pathname-p entry)
	    (delete-directory-tree entry)
	    (delete-file entry)))
	(osicat:delete-directory directory))))

(defun create-directory (pathspec &key (require-fresh-directory nil))
  (port-path:with-directory-form ((directory pathspec))
    (let ((absolute-directory (merge-pathnames directory)))
      (multiple-value-bind (path created)
	  (ensure-directories-exist (merge-pathnames absolute-directory))
	(if (and require-fresh-directory (not created))
	    (error "Could not create fresh directory ~S, already existent."
		   absolute-directory)
	    path)))))