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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.22 2006/02/18 16:18:42 florenz Exp $

;; Main module. Basic operations of gennf are implemented in this file.

(in-package :gennf)

;; For development purposes only.
(defparameter *devel-root*
  "florenz@fiesta.cs.tu-berlin.de:/home/f/florenz/gennf-junk")
(defparameter *devel-access*
  (make-instance 'access :root *devel-root*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(optimize (cl:debug 2))))
;; End of development only section.

(defun create-empty-branch (module access
			    &key (symbolic-name "") (description ""))
  "Create a new branch. That is to create a branch directory
with the next free number and an empty CHANGE file."
  (in-temporary-directory ()
    (create-meta-directory)
    (in-meta-directory
      (let ((branch-directory (make-pathname)))
	(retry ((backend-outdated-error
		 :cleanup (progn (delete-file *branch-file*)
				 (delete-directory-tree
				  branch-directory))))
	    (backend-get module access
			 (list *branch-file* *access-file*) *meta-directory*)
	    (let* ((identifier (get-new-branch-identifier *branch-file*))
		   (branch (make-instance 'branch
			    :identifier identifier
			    :symbolic-name symbolic-name
			    :description description))
		   (change-file (make-pathname)))
	      (setf branch-directory
		    (make-pathname :directory
				   (list :relative
					 (format nil "~A" identifier))))
	      (setf change-file
		    (merge-pathnames branch-directory *change-file*))
	      (add-branch branch *branch-file*)
	      (create-directory branch-directory)
	      (create-new-change-file change-file)
	      (backend-commit module "create-empty-branch" access
			      (list change-file *branch-file*)))))
	(remove-meta-directory))))

(defun create-empty-repository (module access)
  "Create a completely empty repository only containing an
ACCESS and BRANCH file.
FIXME: It should be checked if module already exists."
  (in-temporary-directory ()
    (create-meta-directory)
    (in-meta-directory
      (create-new-branch-file)
      (add-access access *access-file*)
      (backend-import module access))))

(defun checkout (module access branch &optional change)
  "Checkout a change into a sandbox. If no change number is given,
the latest change is checked out.
The sandbox is called *meta-directory* and contains the branch and
access file and the branch subdirectory with it's change file."
  (create-meta-directory)
  (in-meta-directory
    (let* ((branch-directory (branch-identifier-to-directory branch))
	   (change-file (merge-pathnames branch-directory *change-file*)))
      ;; Get change, branch and access file.
      (backend-get module access
		   (list *access-file* *branch-file* change-file)
		   *meta-directory*)
      ;; If no change is given, take latest.
      (unless change
	(setf change (length (read-change-file change-file))))
      ;; Extract the files with revisions to check out and
      ;; exchange the filenames by "branch/filename".
      (let ((files (branch-prefix-file-list
		    (extract-files-and-revisions change-file
						 :identifier change)
		    branch-directory)))
	;; Retrieve the files into *meta-directory*.
	(backend-get module access files *meta-directory*)
	;; Write sandbox-file.
	(write-sandbox-file
	 (make-instance 'sandbox
			:module module
			:branch branch
			:change change
			:access (identifier access)))))))

(defun update (module access branch files &optional change)
  "Update files of a previously checked out change. A sandbox has to exist
and *meta-directory* has to be set properly.
Optionally a change can be passed. If so, files are updated to the
state they had in that change.
As update has to get the list of files to update it is not out of
the box able to retrieve intermittently added files."
  (in-meta-directory
    (let* ((branch-directory (branch-identifier-to-directory branch))
	   (change-file (merge-pathnames branch-directory *change-file*))
	   (change-file-to-get change-file)
	   (old-changes (read-change-file change-file))
	   (sandbox (read-sandbox-file))
	   (old-change-identifier (change sandbox)))
      (setf files (mapcar #'pathname files))
      ;; Set change-file to a filename-revision pair if the
      ;; optional change argument was passed.
      (when change
	(when (< change old-change-identifier)
	  (error "Can not update to a state in the past."))
	(setf change-file-to-get (cons change-file change)))
      ;; Get changes done in the meantime.
      (backend-get module access
		   (list change-file-to-get) *meta-directory*)
      (let* ((changes (read-change-file change-file))
	     ;; Get the list of modified files with their
	     ;; revisions and check, if any of them is to be updated
	     ;; and prefix them with the branch-prefix.
	     (modified-files (get-modified-files old-changes changes))
	     (files-to-update (intersection modified-files
					    files :test #'equal))
	     (files-to-update-and-revisions
	      (extract-files-and-revisions changes :files files-to-update))
	     (files-to-update-branch-prefixed
	      (branch-prefix-file-list files-to-update-and-revisions
				       branch-directory))
	     ;; Get the common predecessors of the files which
	     ;; are to be updated to perform a three-way-merge.
	     (ancestor-files-and-revisions
	      (extract-files-and-revisions old-changes
					   :identifier old-change-identifier
					   :files files-to-update))
	     (ancestor-files-and-revisions-branch-prefixed
	      (branch-prefix-file-list ancestor-files-and-revisions
				       branch-directory)))
	;; If no particular change is required it can be set
	;; from the freshly read changes file.
	(unless change
	  (setf change (length changes)))
	(debug
	  (debug-format "Update following files: ~S"
			files-to-update-branch-prefixed))
	;; Checkout the new revisions of the files to
	;; a temporary directory.
	(in-temporary-directory (temporary-directory)
	  ;; Create one directory for the new revisions of files
	  ;; and one for the ancestor revisions. The latter are
	  ;; necessary because they might be changed in the sandbox.
	  (let ((new-directory
		 (merge-pathnames (make-pathname
				   :directory (list :relative "new"))
				  temporary-directory))
		(ancestor-directory
		 (merge-pathnames (make-pathname
				   :directory (list :relative "ancestor"))
				  temporary-directory)))
	    (create-directory new-directory)
	    (create-directory ancestor-directory)
	    ;; Get the new and ancestor files.
	    (backend-get module access
			 files-to-update-branch-prefixed
			 new-directory)
	    (backend-get module access
			 ancestor-files-and-revisions-branch-prefixed
			 ancestor-directory)
	    ;; Merge the changes into the sandbox.
	    (dolist (file files-to-update-branch-prefixed)
	      (let ((ancestor-file (merge-pathnames (car file)
						    ancestor-directory))
		    (new-file (merge-pathnames (car file)
					       new-directory))
		    (old-file (merge-pathnames (car file)
					       *meta-directory*)))
		(multiple-value-bind (merged-file conflict)
		    (three-way-merge ancestor-file new-file old-file)
		  (when conflict
		    (format t "Conflicts updating file ~S." (car file)))
		  (list-to-file merged-file old-file))))))
	;; Write updated sandbox file.
	(setf (change sandbox) change)
	(write-sandbox-file sandbox)))))
		     
(defun commit (module access branch files)
  "Commit files to the checked out branch. An appropriate change
record is stored. All files must have been changed. If not
they get recorded in the commit but are not assigned a new revision
number which makes the mapping occurence<-->revision-number illegal.
This means, some other functions has to check which files go upstream
before calling this routine."
  (in-meta-directory
    (let* ((branch-directory (branch-identifier-to-directory branch))
	   (change-file (merge-pathnames branch-directory *change-file*))
	   (old-changes (read-change-file change-file))
	   (sandbox (read-sandbox-file)))
      (setf files (mapcar #'pathname files))
      (let* ((changes (retrieve-latest-changes module access branch))
	     (identifier (get-new-change-identifier changes))
	     (change (make-instance 'change
				    :identifier identifier
				    :file-map (create-new-file-map)))
	     (modified-files (get-modified-files old-changes changes))
	     (conflicting-files (intersection files modified-files
					      :test #'equal))
	     ;; If no files are added existing-files equals files.
	     (existing-files (intersection files
					   (all-changed-files changes)
					   :test #'equal)))
	(if conflicting-files
	    ;; Signal an error, if files conflict.
	    (error "The following files conflict: ~S" conflicting-files)
	    ;; No conflicts.
	    (let ((branch-prefixed-existing-files (branch-prefix-file-list
						   existing-files
						   branch-directory))
		  (branch-prefixed-files (branch-prefix-file-list
					  files
					  branch-directory)))
	      (setf changes (add-change change changes))
	      ;; Add files to new change.
	      (dolist (file files)
		(setf changes (add-file-to-changes file changes)))
	      ;; Write new change file.
	      (write-change-file changes change-file)
	      ;; Now, it gets a little bit awkward.
	      ;; As updates are handled by gennf and not by cvs,
	      ;; files in the sandbox may have outdated cvs revisions
	      ;; though they are up to date (which is known from the
	      ;; changes file).
	      ;; To circumvent a cvs update on files in the sandbox,
	      ;; which could causes undesired merges, all work is done
	      ;; in a temporary directory.
	      ;; The last version of all files to be checked in is
	      ;; checked-out to this temporary-directory. Then the
	      ;; files from the sandbox are written to those files
	      ;; and the commit is performed.
	      (in-temporary-directory (temporary-directory)
		(debug
		  (debug-format "Doing commit in directory: ~S"
				temporary-directory)
		  (break))
		(backend-get module access
			     (append (list change-file)
				     branch-prefixed-existing-files)
			     temporary-directory)
		(copy-file (merge-pathnames change-file *meta-directory*)
			   (merge-pathnames change-file temporary-directory)
			   :overwrite t)
		(dolist (file branch-prefixed-files)
		  (copy-file (merge-pathnames file *meta-directory*)
			     (merge-pathnames file temporary-directory)
			     :overwrite t))
		(debug
		  (break))
		(backend-commit module "commit" access
				(append (list change-file)
					branch-prefixed-files)))
	      ;; Write new sandbox file.
	      (debug
		(debug-format "New change identifier is ~S." identifier))
	      (setf (change sandbox) identifier)
	      (write-sandbox-file sandbox)))))))

;; (defun merge (module root branch
;; 	      origin-root origin-branch &optional origin-change)
;;   "Merge is a repository only operation.
;; origin-* indicate the change to merge in. A record the access file
;; is made if necessary.
;; The merge is applied to module and branch on root.
;; The origin's module has to have the same name. The branch
;; has to exist."
;;   (in-temporary-directory
;;     (let* ((access (make-instance 'access :root root))
;; 	   (origin-access (make-instance 'access :root origin-root))
;; 	   (destination-directory (merge-pathnames
;; 				   (make-pathname :directory
;; 						  (list "destination"))))
;; 	   (origin-directory (merge-pathnames
;; 			      (make-pathname :directory
;; 					     (list "origin"))))
;; 	   (destination-branch (merge-pathnames
;; 				(make-pathname :directory
;; 					       (format nil "~S" branch))))
;; 	   (origin-branch (merge-pathnames
;; 			   (make-pathname :directory
;; 					  (format nil "~S" origin-branch)))))
;;       (create-directory destination-directory :require-fresh-directory t)
;;       (create-directory origin-directory :require-fresh-directory t)
;;       (in-directory (destination-directory)
;; 	(checkout module root branch))
;;       (in-directory (origin-directory)
;; 	(checkout module origin-root origin-branch origin-change))
