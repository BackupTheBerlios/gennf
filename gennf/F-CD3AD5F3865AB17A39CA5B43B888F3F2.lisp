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
;; $Id: F-CD3AD5F3865AB17A39CA5B43B888F3F2.lisp,v 1.10 2006/03/05 18:48:15 florenz Exp $

;; All directory related functions and macros live in this file.
;; This includes changing working directory, moving and deletion
;; of directory trees.
;; Many of those functions should be generalized as much as possible
;; and go in port-path someday.

(in-package :gennf)

(defmacro in-meta-directory (&body forms)
  "Evaluate forms with *meta-directory* being current
directory and change back to previous working directory
afterwards."
  (let ((current-directory (gensym "current-directory-")))
    `(let ((,current-directory (current-directory)))
      (change-to-meta-directory)
      ,@forms
      (port-path:change-directory ,current-directory))))

(defun create-meta-directory ()
  "Create *meta-directory* and signal a condition, if it
is already there. Set *meta-directory* to path
of the created directory."
  (port-path:create-directory *meta-directory-name* :require-fresh-directory t)
  (setf *meta-directory* (merge-pathnames *meta-directory-name*)))

(defun change-to-meta-directory ()
  "Make *meta-directory* current working directory."
  (port-path:change-directory *meta-directory*))

(defun remove-meta-directory ()
  "Delete *meta-directory* and all its subdirectories
and files."
  (when (port-path:path-exists-p *meta-directory*)
    (change-to-meta-directory)
    (port-path:change-directory-up)
    (port-path:delete-directory-tree *meta-directory*)))

(defun find-meta-directory ()
"finds from current directory ongoing the higher \"META\" direcory"
  (first
   (port-path:search-directory-in-directories "META"
					      (port-path:parent-dirs *default-pathname-defaults*))))