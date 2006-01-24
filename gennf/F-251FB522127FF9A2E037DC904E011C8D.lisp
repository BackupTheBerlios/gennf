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
;; $Id: F-251FB522127FF9A2E037DC904E011C8D.lisp,v 1.17 2006/01/24 19:33:27 florenz Exp $

;; BOGUS2345678901234

(in-package :gennf)

(define-condition backend-error (error)
  ((code :initarg :code :reader code)
   (description :initarg :description :reader description)))

(define-condition backend-outdated-error (backend-error)
  ((files :initarg :files :reader files)))

(defun backend-get (module access files destination)
  (let ((backend (extract :backend access)))
    (cond ((eql backend :cvs) (cvs-get module access files destination))
	  (t (error "Backend ~S not implemented." backend)))))

(defun backend-import (module access)
  (let ((backend (extract :backend access)))
    (cond ((eql backend :cvs) (cvs-import module access))
	  (t (error "Backend ~S not implemented." backend)))))

(defun backend-commit (module access files)
  "files are commited to the given repository.
file pathnames have to be relative to the given working
directory and must be in file-form. All files and
the directories they are stored in have to exist.
This means in particular that it is not possible to
commit empty directories.

If it is not possible to commit a file because
it is outdated a backend-outdated-error is signalled.
No file will committed in this case."
  (let ((backend (extract :backend access)))
    (cond ((eql backend :cvs)
	   (cvs-commit module access files))
	  (t (error "Backend ~S not implemented." backend)))))
