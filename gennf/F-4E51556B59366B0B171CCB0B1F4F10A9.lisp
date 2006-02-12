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
;; $Id: F-4E51556B59366B0B171CCB0B1F4F10A9.lisp,v 1.24 2006/02/12 14:26:16 florenz Exp $

;; All functions that interact with CVS directly live in
;; this file. These routines are only called from backend.lisp
;; and from no where else.

(in-package :gennf)

(defparameter *cvs-meta-directory*
  (make-pathname :directory (list :relative "CVS"))
  "Relative path of cvs meta directory.")
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

(defun cvs-output-to-file (arguments file)
  "Run cvs command with given arguments. All output to
standard output is written to file. Error handling is like
cvs-default-error-handling."
  (with-cvs-output (arguments :exit-code exit-code :output output)
    (ensure-directories-exist file)
    (with-open-file (destination file
				 :direction :output
				 :if-exists :supersede
				 :if-does-not-exist :create)
      (if (= exit-code 0)
	  (loop for line = (read-line output nil)
		while line
		do (write-line line destination))
	  (error 'backend-error :code exit-code
		 :description
		 "Something went wrong with cvs-output-to-file.")))))

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

(defun cvs-remove-sticky-tag (access file)
  "Remove the sticky tag from file that results from
cvs up or cvs co with -r option."
  (apply #'cvs-default-error-handling
	 (list "-d" (extract :root access)
	       "up" "-A" (namestring file))))

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
  "Each element of files may be an atom or a dotted pair
as described for function backend-get.
cvs-get either calls cvs-get-checkout or cvs-get-update,
depending on the existance of *cvs-meta-directory*
in destination."
  (if (port-path:path-exists-p (merge-pathnames *cvs-meta-directory*
						destination))
      (cvs-get-update access files destination)
      (cvs-get-checkout module access files destination)))

(defun cvs-get-update  (access files destination)
  "Checks out all given files to destination. destination
has to be a cvs sandbox, i. e. it contains *cvs-meta-directory*"
  (unless files (return-from cvs-get-update)) ; No files, do nothing.
  (in-directory destination
    (let ((default-argument-list (list "-d" (extract :root access) "up")))
      (dolist (file files)
	(let ((argument-list
	       (append default-argument-list
		       (cvs-file-revision-argument file)))
	      (output-file (cvs-output-file file)))
	  (apply #'cvs-default-error-handling argument-list)
	  (cvs-remove-sticky-tag access output-file))))))

(defun cvs-get-checkout (module access files destination)
  "For some reason co -d . ist not possible with cvs using
ssh as transport layer. That means that all files
are checked out into <tmp>/module/. The whole content of
<tmp>/module is moved to destination,
especially including the cvs meta directory, because
cvs-commit would not work otherwise.
Caution: it is not possible to get a proper sandbox
by repeatedly calling cvs-get-checkout. This is due to the circumstance
that files are checked out to a temporary storage and moved
to the destination. This includes cvs meta data. Subsequently
calling cvs-get-checkout for the same destination results in meta data
that is only consistent for the last chunk that was checked out."
  (unless files (return-from cvs-get-checkout)) ; No files, do nothing.
  (in-temporary-directory
    (let ((default-argument-list (list "-d" (extract :root access) "co"))
	  (module-path (make-pathname :directory
				      (list :relative module))))
      ;; Fetch all files.
      (dolist (file files)
	(let ((argument-list
	       (append default-argument-list
		       (cvs-file-revision-argument file module)))
	      (output-file (cvs-output-file file module-path)))
	  ;; Call cvs correct argument list for desired
	  ;; revision of file.
	  (apply #'cvs-default-error-handling argument-list)
	  (cvs-remove-sticky-tag access output-file)))
      ;; Move checked out files to destination
      ;; and delete temporary subdirectory module-path.
      (port-path:with-directory-form ((destination-directory
				       destination))
	(move-directory-tree module-path
			     destination-directory)
	(delete-directory-tree module-path)))))

(defun cvs-output-file (file-request &optional prefix)
  "Extracts a filename from file-request, which may be
a dotted pair (filename . revision) or an atom filename,
to which cvs up or cvs co output is written."
  (multiple-value-bind (file-revision output-file)
      (cvs-file-revision-transform file-request prefix)
    (declare (ignore file-revision))
    output-file))

(defun cvs-file-revision-argument (file-request &optional prefix)
  "If file-request is a dotted pair (file . revision),
proper cvs arguments like (-r1.2 filename) is returned.
If pair is an atom, (filename) is returned.
If a prefix is given, it is prepended to the file's filename."
  (cvs-file-revision-transform file-request prefix))

(defun cvs-file-revision-transform (file-request &optional prefix)
  "Backend for cvs-output-file and cvs-file-revision-transform.
The return value is a pair whose first element is an argument
list for cvs an secon argument is a filename to store write
checked out content to."
  (flet ((prepend-prefix (filename prefix)
	   (if prefix
	       (format nil "~A/~A" prefix (namestring filename))
	       (namestring filename)))
	 (output-pathname (filename prefix)
	   (if prefix
	       (merge-pathnames (pathname filename) (pathname prefix))
	       (pathname filename))))
    (if (consp file-request)
	;; file is a dotted pair (filename . revision).
	(let ((filename (prepend-prefix (car file-request) prefix))
	      (revision (change-revision-to-cvs-revision (cdr file-request)))
	      (output-file (output-pathname (car file-request) prefix)))
	  (values (list revision filename) output-file))
      ;; file is an atom.
      (values (list (prepend-prefix file-request prefix))
	      (output-pathname file-request prefix)))))
  
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
