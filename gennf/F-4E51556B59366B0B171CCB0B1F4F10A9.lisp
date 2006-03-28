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
;; $Id: F-4E51556B59366B0B171CCB0B1F4F10A9.lisp,v 1.39 2006/03/28 14:11:45 sigsegv Exp $

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
cvs-default-error-handling.
This is useful with the -p option to co or up,
which emits files on standard-output."
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
	(t (error 'backend-error :description "cvs had non 0 exit code."
		  :code exit-code))))

(defun invoke-cvs (&rest arguments)
  "Run cvs program with given arguments."
  (debug
    (debug-format "cvs invoked with following arguments:~%~A"
		  arguments))
  (invoke-program-silently *cvs-command-name* arguments))

(defun cvs-default-error-handling (&rest args)
  "Default error handling is to signal a condition if
cvs exists with a non 0 exit code."
  (cvs-default-error-handler (apply #'invoke-cvs args)))

(defun cvs-remove-sticky-tag (access file)
  "Remove the sticky tag from file that results from
cvs up or cvs co with -r option.
Note: changes stored for this file in the repository
are incorporated in the checked out copy."
  (apply #'cvs-default-error-handling
	 (list "-d" (root access)
	       "up" "-A" (namestring file))))

(defun change-revision-to-cvs-revision (revision)
  "Converts the revision number stored in a change,
a natural number, into a CVS revision string:
23 gets -j1.23."
  (format nil "-j1.~S" revision))

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
proper cvs arguments like (-j1.2 filename) is returned.
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

(defun cvs-up-to-date-p (access file)
  "Return if the checked out copy of file is
an up to date copy (or a modification of an up
to ate copy(."
  (port-path:ensure-string-pathname file)
  (with-cvs-output ((list "-d" (root access)
			  "-n" "up" file)
		    :output stream)
    (let ((output (read-line stream nil)))
      (not (and output 
		(char/= (aref output 0) #\A)
		(char/= (aref output 0) #\?))))))
	  
(defun cvs-known-file-p (module access file)
  "Returns if file is known by cvs (if it was already commited)."
    (port-path:in-temporary-directory
	(:temporary-pathname temporary-directory)
      (handler-case
	  (cvs-get module access (list file) temporary-directory)
	(backend-error () (return-from cvs-known-file-p nil))))
    t)

(defun cvs-file-added-p (access file)
  "Checks if file was already added."
  (port-path:ensure-string-pathname file)
  (with-cvs-output ((list "-d" (root access)
			  "log" file)
		    :exit-code exit-code)
    (= exit-code 0)))

(defun cvs-known-module-p (access module)
  "Returns if module is known at access. It is checked if
a branch file is stored for that module. That means 
that a 'normal' cvs repository is not recognized."
  (let ((file (format nil "~A/~A" module (namestring *branch-file-name*)))
	known)
    (port-path:in-temporary-directory ()
      (with-cvs-output ((list "-d" (root access)
			      "co" file)
			:exit-code exit-code)
	(setf known (= exit-code 0))))
    known))
		      
(defun cvs-import (module access)
  "Interface to cvs import command, which is
necessary to create a new repository."
 (when (cvs-known-module-p access module)
    (error 'backend-module-exists-error
	   :module module
	   :description "The module already exists."))
  (cvs-default-error-handling "-d" (root access)
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
in destination.
If something very ugly happens, the *cvs-meta-directory*
in destination is not a suitable one (i. e. calling this
routine for some arbitrary cvs sandbox.)"
  (if (port-path:path-exists-p (merge-pathnames *cvs-meta-directory*
						destination))
      (cvs-get-update access files destination)
      (cvs-get-checkout module access files destination)))

(defun cvs-get-update (access files destination)
  "Checks out all given files to destination. destination
has to be a cvs sandbox, i. e. it contains *cvs-meta-directory*"
  (unless files (return-from cvs-get-update)) ; No files, do nothing.
  (port-path:in-directory destination
    (let ((default-argument-list (list "-d" (root access)
				       "up" "-jHEAD")))
      (dolist (file files)
	(let ((argument-list
	       (append default-argument-list
		       (cvs-file-revision-argument file))))
	  (apply #'cvs-default-error-handling argument-list))))))

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
  (port-path:in-temporary-directory ()
    (let ((default-argument-list (list "-d" (root access)
				       "co" "-jHEAD"))
	  (module-path (make-pathname :directory
				      (list :relative module))))
      ;; Fetch all files.
      (dolist (file files)
	(let ((argument-list
	       (append default-argument-list
		       (cvs-file-revision-argument file module))))
	  ;; Call cvs with correct argument list for desired
	  ;; revision of file.
	  (apply #'cvs-default-error-handling argument-list)))
      ;; Move checked out files to destination
      ;; and delete temporary subdirectory module-path.
      (port-path:with-directory-form ((destination-directory
				       destination))
	(port-path:move-directory-tree module-path
				       destination-directory)
	(port-path:delete-directory-tree module-path)))))
  
(defun cvs-commit (message access files)
  "If files are not known to cvs they are added.
The current working directory has to be a result of a cvs-get.
If some files are outdated a condition backend-outdated-error
is signalled, containing a list of outdated files. In this
case no files are committed at all."
  (let (argument-list)
    (dolist (file files)
      (unless (cvs-file-added-p access file)
	(debug
	  (debug-format "Checking if file ~S is known to cvs." file))
	(cvs-add access (list file))))
    ;; Open temporary file for log message and write log message.
    (port-path:with-temporary-file (message-stream message-file)
      (write-string message message-stream)
      (finish-output message-stream)
      ;; Construct argument list cor cvs.
      (setf argument-list (append (list "-d" (root access)
					"ci" "-F" (namestring message-file))
				  (mapcar #'namestring files)))
      (with-cvs-output (argument-list :error error :exit-code exit-code)
	(unless (= exit-code 0)
	  ;; Check if files were outdated and signal appropriately.
	  (let ((outdated-files
		 (loop with filenames = (mapcar #'namestring files)
		       for line = (read-line error nil)
		       do (format t "~S" line)
		       while line
		       when (search "Up-to-date check failed" line)
		       append (search-multiple filenames line))))
	    (if outdated-files
		(progn
		  (setf outdated-files (mapcar #'pathname outdated-files))
		  (error 'backend-outdated-error :code exit-code
			 :description "Some files are outdated."
			 :files outdated-files))
		(error  'backend-error :code exit-code
			:description "Something wrong with cvs ci."))))))))

(defun cvs-add (access files)
  "Do a cvs add for all files. files have to be in
file-form, no directories! Files may be have a
directory part. In this case, all directories are added
to the repository."
  (dolist (file files)
    (let* ((directory-prefixes (cdr (port-path:pathname-prefixes file)))
	   (paths (append directory-prefixes (list file))))
      (dolist (path paths)
	(port-path:ensure-string-pathname path)
	(invoke-cvs "-d" (root access)
		    "add"
		    path)))))