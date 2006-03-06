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
;; $Id: F-251FB522127FF9A2E037DC904E011C8D.lisp,v 1.24 2006/03/06 15:52:46 florenz Exp $

;; This file is gennf's backend abstraction layer.
;; All interaction with a backend will be by routines of this file.

(in-package :gennf)

(define-condition backend-error (error)
  ((code :initarg :code :reader code
	 :documentation "The exit code of the backend for
the operations which caused signalling of backend-error.")
   (description :initarg :description :reader description
		:documentation "Some text escribing the error."))
  (:documentation "A condition signalled when something went
wrong in the backend."))

(define-condition backend-outdated-error (backend-error)
  ((files :initarg :files :reader files
	  :documentation "List of files which were not up to date."))
  (:documentation "A condition which is signalled when some files
to be checked in are not modifications of the last revision."))

(define-condition backend-module-exists-error (backend-error)
  ((module :initarg :module :reader :module
	   :documentation "Name of outdated module."))
  (:documentation "Condition which is signalled of a new module
which is already there is to be created."))

(defun backend-get (module access files destination)
  "Get the indicated files from access and module and
put them into destination directory.
files is a list, each element may either be a filename
or a dotted pair (filename . revision). Revision is the
number stored in a change. If an element is only a
filename, the latest revision will be fetched."
  (let ((backend (backend access)))
    (cond ((eql backend :cvs) (cvs-get module access files destination))
	  (t (error "Backend ~S not implemented." backend)))))

(defun backend-import (module access)
  (let ((backend (backend access)))
    (cond ((eql backend :cvs) (cvs-import module access))
	  (t (error "Backend ~S not implemented." backend)))))

(defun backend-commit (module message access files)
  "files are commited to the given repository.
File pathnames have to be relative to the given working
directory and must be in file-form. All files and
the directories they are stored in have to exist.
This means in particular that it is not possible to
commit empty directories.
If it is not possible to commit a file because
it is outdated a backend-outdated-error is signalled.
No file will be committed in this case."
  (declare (ignore module))
  (let ((backend (backend access)))
    (cond ((eql backend :cvs)
	   (cvs-commit message access files))
	  (t (error "Backend ~S not implemented." backend)))))
