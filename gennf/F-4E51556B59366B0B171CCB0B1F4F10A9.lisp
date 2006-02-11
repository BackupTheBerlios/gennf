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
;; $Id: F-4E51556B59366B0B171CCB0B1F4F10A9.lisp,v 1.23 2006/02/11 21:20:16 florenz Exp $

;; All functions that interact with CVS directly live in
;; this file. These routines are only called from backend.lisp
;; and from no where else.

(in-package :gennf)

(defparameter *cvs-import-log-message* *backend-import-log-message*
  "The log message recorded when using cvs-import function.")
(defparameter *cvs-import-vendor-tag* "gennf-created"
  "The vendor tag passed to cvs when using cvs-import function.")
(defparameter *cvs-import-release-tag* "fresh-repository"
  "The release tag passed to cvs when using cvs-import function.")
(defparameter *cvs-command-name* "cvs"
  "The name of the cvs binary.")

(defmacro with-cvs-output ((arguments &key exit-code
				      output error) &body forms)
  "Run cvs with given arguments an bind its output
to stream variables output, error if provided and
store the exit-code in exit-code if provided."
  `(with-program-output (*cvs-command-name*
			 ,arguments
			 :output ,output
			 :error ,error
			 :exit-code ,exit-code)
    ,@forms))

(defun cvs-default-error-handler (exit-code)
  "Signals an errorif exit-code is not 0. The
exit-code is mentioned in the error message."
  (cond ((= exit-code 0) exit-code)
	(t (error "cvs had exit code ~S." exit-code))))

(defun invoke-cvs (&rest arguments)
  "Run cvs program with given arguments."
  (invoke-program *cvs-command-name* arguments))

(defun cvs-default-error-handling (&rest args)
  "Default error handling is to signal a condition if
cvs exists with a non 0 exit code."
  (cvs-default-error-handler (apply #'invoke-cvs args)))

(defun change-revision-to-cvs-revision (revision)
  "Converts the revision number stored in a change,
a natural number, into a CVS revision string:
23 gets -r1.23"
  (format nil "-r1.~S" revision))

(defun cvs-import (module access)
  "Interface to cvs import command, which is
necessary to create a new repository."
  (cvs-default-error-handling "-d" (extract :root access)
			      "import"
			      "-m" *cvs-import-log-message*
			      module
			      *cvs-import-vendor-tag*
			      *cvs-import-release-tag*))

(defun cvs-get (module access files destination)
  "For some reason co -d . ist not possible with cvs using
ssh as transport layer. That means that all files
are checked out into <tmp>/module/. The whole content of
<tmp>/module is moved to destination,
especially including the cvs meta directory, because
cvs-commit would not work otherwise.
Each element of files may be an atom or a dotted pair
as described for function backend-get."
  (unless files (return-from cvs-get))
  (in-temporary-directory
    (let* ((default-argument-list (list "-d" (extract :root access) "co"))
	   (module-path (make-pathname :directory
				       (list :relative module))))
      ;; Fetch all files.
      (dolist (file files)
	(let ((argument-list default-argument-list))
	  (if (consp file)
	      ;; file is a dotted pair (filename . revision).
	      (let ((filename (car file))
		    (revision (cdr file)))
		(setf filename (format nil "~A/~A" module
				       (namestring filename)))
		(setf revision (change-revision-to-cvs-revision revision))
		(setf argument-list (append default-argument-list
					    (list revision filename))))
	      ;; file is an atom.
	      (progn
		(setf file (format nil "~A/~A" module
				   (namestring file)))
		(setf argument-list (append default-argument-list
					    (list file)))))
	  ;; Call cvs correct argument list for desired
	  ;; revision of file.
	  (apply #'cvs-default-error-handling argument-list)))
      ;; Move checked out files to destination
      ;; and delete temporary subdirectory module-path.
      (port-path:with-directory-form ((destination-directory
				       destination))
	(move-directory-tree module-path
			     destination-directory)
	(delete-directory-tree module-path)))))

(defun cvs-commit (message access files)
  "If files are not known to cvs they are added.
The current working directory has to be a result of a cvs-get.
If some files are outdated a condition backend-outdated-error
is signalled, containing a list of outdated files. In this
case no files are committed at all."
  (let (argument-list)
    (dolist (file files)
      (unless (cvs-known-file-p access file)
	(cvs-add access (list file))))
    (setf argument-list (append (list "-d" (extract :root access)
				      "ci" "-m" message)
				(mapcar #'namestring files)))
    (break)
    (with-cvs-output (argument-list :error error :exit-code exit-code)
      (unless (= exit-code 0)
	(break)
	;; Check if files were outdated and signal appropriately.
	(let ((outdated-files
	       (loop for line = (read-line error nil)
		     while line
		     when (search "Up-to-date check failed" line)
		     append (search-multiple files line))))
	  (if outdated-files
	      (progn
		(setf outdated-files (mapcar #'pathname outdated-files))
		(error 'backend-outdated-error :code exit-code
		       :description "Some files are outdated."
		       :files outdated-files))
	      (error  'backend-error :code exit-code
		      :description "Something went wrong with cvs ci.")))))))

(defun cvs-up-to-date-p (access file)
  "Return if the checked out copy of file is
an up to date copy (or a modification of an up
to ate copy(."
  (ensure-string-pathname file)
  (with-cvs-output ((list "-d" (extract :root access)
			  "-n" "up" file)
		    :output stream)
    (let ((output (read-line stream nil)))
      (not (and output 
		(char/= (aref output 0) #\A)
		(char/= (aref output 0) #\?))))))
	  
(defun cvs-known-file-p (access file)
  "Returns if file is known by cvs (if it is already added)."
  (ensure-string-pathname file)
  (with-cvs-output ((list "-d" (extract :root access)
			  "log" file)
		    :exit-code exit-code)
    (= exit-code 0)))

(defun cvs-add (access files)
  "Do a cvs add for all files. files have to be in
file-form, no directories! Files may be have a
directory part. In this case, all directories are added
to the repository."
  (dolist (file files)
    (let* ((directory-prefixes (cdr (port-path:pathname-prefixes file)))
	   (paths (append directory-prefixes (list file))))
      (dolist (path paths)
	(ensure-string-pathname path)
	(invoke-cvs "-d" (extract :root access)
		    "add"
		    path)))))
