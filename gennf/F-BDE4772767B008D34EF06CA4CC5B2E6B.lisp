;; Copyright 2006 Florian Lorenzen, Fabian Otto
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
;; $Id: F-BDE4772767B008D34EF06CA4CC5B2E6B.lisp,v 1.1 2006/03/24 14:10:34 sigsegv Exp $

;; This file contains routines to manipulate checkpoint files.


(in-package :gennf)

(defclass checkpoint ()
  ((files 
    :initarg :files
    ;;    :initform '()			
    :accessor files)
   (module
    :initarg :module
    :accessor module)
   (root
    :initarg :root
    :accessor root)
   (branch 
    :initarg :branch
    :accessor branch))
  (:documentation "checkpoint contains confilicted files"))

(defmethod convert-to-alist append ((checkpoint checkpoint))
  "Converts a checkpoint object to an alist"
  (acons :files (files checkpoint)
	 (acons :module (module checkpoint)
		(acons :root (root checkpoint)
		       (acons :branch (branch checkpoint) ())))))

(defun alist-to-checkpoint (alist)
""
;; FIXME: Test if alist is a checkpoint
  (make-instance 'checkpoint
		 :files (extract :files alist)
		 :module (extract :module alist)
		 :root (extract :root alist)
		 :branch (extract :branch alist)))

(defmethod print-object ((checkpoint checkpoint) stream)
  (if *print-readably*
      (call-next-method)
      (prin1 (convert-to-alist checkpoint) stream)))


(defun read-checkpoint-file (&optional (file *checkpoint-file-name*))
  "Reads an checkpoint file and returns a sequence of
accesss objects."
  (alist-to-checkpoint (read-file file)))


(defun write-checkpoint-file (sequence &optional (file *checkpoint-file-name*))
  "Writes the sequence of checkpoint objects to file."
  (prin1-file file sequence))
