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
;; $Id: F-74178C2E50AE1257726E1B3D58FE1EEE.lisp,v 1.18 2006/03/17 14:08:47 florenz Exp $

;; Basic operations for changes and distributed repositories are
;; implemented in this file.

(in-package :gennf)

(defun create-empty-branch (module access
			    &key (symbolic-name "") (description ""))
  "Create a new branch. That is to create a branch directory
with the next free number and an empty change file.
It returns the identifier of the branch created."
  (let (identifier) ; This is just because it is to be returned.
    (port-path:in-temporary-directory ()
      (create-meta-directory)
      (in-meta-directory
	(let ((branch-directory (make-pathname)))
	  (retry ((backend-outdated-error
		   :cleanup (progn (delete-file *branch-file-name*)
				   (port-path:delete-directory-tree
				    branch-directory))))
	    (backend-get module access
			 (list *branch-file-name* *access-file-name*)
			 *meta-directory*)
	    (setf identifier (get-new-branch-identifier *branch-file-name*))
	    (let* ((branch (make-instance 'branch
					  :identifier identifier
					  :symbolic-name symbolic-name
					  :description description))
		   (change-file (make-pathname))
		   (map-file (make-pathname)))
	      (setf branch-directory
		    (branch-identifier-to-directory identifier))
	      (setf change-file
		    (merge-pathnames branch-directory *change-file-name*))
	      (setf map-file
		    (merge-pathnames branch-directory *map-file-name*))
	      (add-branch branch *branch-file-name*)
	      (port-path:create-directory branch-directory)
	      (create-new-change-file change-file)
	      (add-file-to-changes *map-file-name* change-file)
	      (break)
	      (create-new-map-file map-file) ; creates empty map file,
					     ; containing only "NIL"
	      (backend-commit module *log-empty-branch* access
			      (list change-file *branch-file-name*
				    map-file)))))
	(remove-meta-directory)))
    identifier))

(defun create-empty-repository (module access)
  "Create a completely empty repository only containing an
access and branch file. If the module already exists an error is
signalled."
  (port-path:in-temporary-directory ()
    (create-meta-directory)
    (in-meta-directory
      (create-new-branch-file)
      (add-access access *access-file-name*)
      (backend-import module access))))

(defun distribution-checkout (module access branch &optional change)
  "Checkout a change into a sandbox. If no change number is given,
the latest change is checked out.
The sandbox is called *meta-directory* and contains the branch and
access file and the branch subdirectory with it's change file."
  (create-meta-directory)
  (in-meta-directory
    (let* ((branch-directory (branch-identifier-to-directory branch))
	   (change-file (merge-pathnames branch-directory *change-file-name*))
	   (map-file (merge-pathnames branch-directory *map-file-name*)))
      ;; Get change, branch, map and access file.
      (backend-get module access
		   (list *access-file-name* *branch-file-name*
			 change-file map-file)
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
	(format t "files: ~a~%" files)
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
	   (change-file (merge-pathnames branch-directory *change-file-name*))
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
	(port-path:in-temporary-directory
	    (:temporary-pathname temporary-directory)
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
	    (port-path:create-directory new-directory)
	    (port-path:create-directory ancestor-directory)
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

(defun repository-changed-files (module access branch &key files change)
  "Return a list of files that have changed in the repository relative
to the current sandbox. If a change is given, only changes between the
given change and the sandbox are considered. If a list of files is
provided, the result is the intersection of this file list and
the changed files from the repository."
  (in-meta-directory
   (let* ((branch-directory (branch-identifier-to-directory branch))
	  (change-file (port-path:append-pathnames *meta-directory*
						  branch-directory
						  *change-file-name*))
	  (sandbox-changes (read-change-file change-file))
	  (repository-changes
	   (let ((latest-changes (retrieve-latest-changes module
							  access branch)))
	     (if change
		 (nthcdr (- (length latest-changes) change) latest-changes)
		 latest-changes)))
	  (changed-files (get-modified-files sandbox-changes
					     repository-changes)))
     (if files
	 (intersection files changed-files)
	 changed-files))))

(defun sandbox-changed-files (module access branch &optional files)
  "Returns a list fo files in the sandbox which have changes.
All files below the branch directory are considered except
update special meta files. The file list is suitable as argument for commit.
If an optional file list is given, the check is restricted to
the intersection of files and the directory listing in the meta directory.
Rationale: The user may want to restrict update to certain files."
  (in-meta-directory
    ;; This function is used to remove files like the change file
    ;; from the update list because these files have to be treated specially.
    (flet ((filter-files (file-list)
	     (remove-if #'(lambda (file)
			    (listed-file-p file *update-special-meta-files*)) 
			file-list)))
      (let* ((changed-files ()) ; This will be the result.
	     (branch-directory (branch-identifier-to-directory branch))
	     ;; All files in meta directory (no subdirectories).
	     (all-files
	      (remove-if #'port-path:directory-pathname-p
			 (port-path:directory-listing branch-directory)))
	     ;; Filter out special files and restrict to user's
	     ;; update wish-list.
	     (updatable-files 
	      (let ((filtered-files (filenames-only (filter-files all-files))))
		(if files
		    (intersection (filenames-only files)
				  filtered-files :test #'equal)
		    filtered-files)))
	     (updatable-files-absolute
	      (mapcar #'(lambda (filename)
			  (port-path:append-pathnames *meta-directory*
						      branch-directory
						      filename))
		      updatable-files))
	     (updatable-files-branch-prefixed
	      (branch-prefix-file-list updatable-files branch-directory)))
	(port-path:in-temporary-directory
	    (:temporary-pathname temporary-directory
				 :always-cleanup nil)
	  (debug
	    (debug-format "Doing up-to-date check in ~A."
			  temporary-directory))
	  ;; Iterate over all possibly changed files, get
	  ;; latest revision from repository and check if
	  ;; changes occured.
	  ;; This could also be performed using the backend's
	  ;; diffing mechanism but this not done for two reasons:
	  ;; 1. It is not desireable to have two different diff
	  ;; algorithm in the program.
	  ;; 2. CVS as backend has to fetch all files, too, to calculate
	  ;; the differences. Furthermore, program output parsing is
	  ;; quite error prone.
	  (dotimes (file-number (length updatable-files))
	    (let ((fresh-file (nth file-number
				   updatable-files-branch-prefixed))
		  (aged-file (nth file-number
				  updatable-files-absolute))
		  (current-filename (nth file-number
					 updatable-files)))
	      (handler-case
		  (progn
		    (backend-get module access (list fresh-file)
			       temporary-directory)
		    (when
			(not-same-p aged-file
				    (merge-pathnames fresh-file
						     temporary-directory))
		      (push current-filename changed-files)
		      (debug
			(debug-format "Put file ~A into change list."
				      current-filename))))
		(backend-error () (push current-filename changed-files))))))
	changed-files))))

(defun distribution-commit (module message access branch files)
  "Commit files to the checked out branch. An appropriate change
record is stored. All files must have been changed. If not
they get recorded in the commit but are not assigned a new revision
number which makes the mapping occurence<-->revision-number illegal.
This means, some other functions has to check which files go upstream
before calling this routine."
  (in-meta-directory
    (let* ((branch-directory (branch-identifier-to-directory branch))
	   (change-file (merge-pathnames branch-directory *change-file-name*))
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
	      (port-path:in-temporary-directory
		  (:temporary-pathname temporary-directory)
		(debug
		  (debug-format "Doing commit in directory: ~S"
				temporary-directory))
		(backend-get module access
			     (append (list change-file)
				     branch-prefixed-existing-files)
			     temporary-directory)
		(port-path:copy-file (merge-pathnames change-file
						      *meta-directory*)
				     (merge-pathnames change-file
						      temporary-directory)
			   :overwrite t)
		(dolist (file branch-prefixed-files)
		  (port-path:copy-file (merge-pathnames file *meta-directory*)
				       (merge-pathnames file
							temporary-directory)
			     :overwrite t))
		(backend-commit module message access
				(append (list change-file)
					branch-prefixed-files)))
	      ;; Write new sandbox file.
	      (debug
		(debug-format "New change identifier is ~S." identifier))
	      (setf (change sandbox) identifier)
	      (write-sandbox-file sandbox)))))))

(defun merge (module access branch
	      origin-access origin-branch &optional origin-change)
  "Merge is a repository only operation.
origin-* indicate the change to merge in. A record to the access file
is made if necessary.
The merge is applied to module and branch on access.
The origin's module has to have the same name. The branch
has to exist.
The merge is performed in a temporary directory. If conflicts
occur when merging the files, the temporary directory is kept
and the function returns its pathname as well as the files
which were not commited as multiple valyes. The conflicts then have to
be resolved by hand and merge-finish has to be called on this
directory and files.
If no conflicts happen merge returns NIL and temporary data is
deleted (no conflicts is the ususal case when using merge for
branching."
  (port-path:in-temporary-directory
      (:temporary-pathname temporary-directory
       :always-cleanup nil)
    ;; In temporary-directory two directories are created:
    ;; destination and origin. The data to be merged in is
    ;; stored in origin and the latest change of the branch
    ;; which will receive the merge is stored in destination.
    (let* ((destination-directory (merge-pathnames *merge-destination*
						   temporary-directory))
	   (destination-branch-directory (branch-identifier-to-directory
					  branch))
	   (destination-branch-absolute (merge-pathnames
					 destination-branch-directory
					 destination-directory))
	   (origin-directory (merge-pathnames *merge-origin*
					      temporary-directory))
	   (origin-branch-directory (branch-identifier-to-directory
				     origin-branch))
	   (origin-branch-absolute (merge-pathnames origin-branch-directory
						    origin-directory))
	   (destination-changes (retrieve-latest-changes module
							 access
							 branch))
	   (destination-files-and-revisions
	    (extract-files-and-revisions destination-changes))
	   (origin-changes (retrieve-latest-changes module
						    origin-access
						    origin-branch))
	   (origin-files-and-revisions
	    (extract-files-and-revisions origin-changes
					 :identifier origin-change))
	   (origin-files-and-revisions-prefixed
	    (branch-prefix-file-list origin-files-and-revisions
				     origin-branch-directory))
	   (origin-files
	    (remove-revisions origin-files-and-revisions))
	   ;; This will be the final new change sequence
	   (new-changes destination-changes)
	   (destination-change-file (merge-pathnames
				     destination-branch-directory
				     *change-file-name*))
	   ;; Common files have to be merged.
	   ;; Can it be relied on that the elements from
	   ;; intersection's first argument go to the result?
	   ;; If not, this has to be rewritten, because revision
	   ;; numbers from destination habe to be in
	   ;; common-files-and-revisions.
	   (common-files-and-revisions
	    (intersection destination-files-and-revisions
			  origin-files-and-revisions
			  :test #'equal
			  :key #'car))
	   (common-files-and-revisions-prefixed
	    (branch-prefix-file-list common-files-and-revisions
				     destination-branch-directory))
	   (common-files
	    (remove-revisions common-files-and-revisions))
	   ;; Uncommon files are just added.
	   (uncommon-files
	    (set-difference origin-files common-files :test #'equal)))
      (unless origin-change
	(setf origin-change (length origin-changes)))
      (debug
        (debug-format "Destination files are in ~S.
Origin files are in ~S."
		      destination-directory origin-directory))
      (port-path:create-directory destination-directory
				  :require-fresh-directory t)
      (port-path:create-directory origin-directory :require-fresh-directory t)
      (debug 
        (debug-format "DESTINATION CHANGES ~S" destination-changes))
      ;; Only the common files have to be fetched from the destination
      ;; branch because only those have to be merged. The other
      ;; files can just be added.
      ;; The change file has to be fetched too because it will be
      ;; fed up again.
      (backend-get module access
		   (cons destination-change-file
			 common-files-and-revisions-prefixed)
		   destination-directory)
      (backend-get module origin-access origin-files-and-revisions-prefixed
		   origin-directory)
      (let ((conflicts nil))
	;; Copy uncommon files.
	(dolist (file uncommon-files)
	  (let ((complete-filename (merge-pathnames file
						    origin-branch-absolute)))
	    (port-path:copy-file complete-filename
				 destination-branch-absolute)))
	;; Merge common files.
	(dolist (file common-files)
	  (let ((origin-file (merge-pathnames file
					      origin-branch-absolute))
		(destination-file (merge-pathnames
				   file
				   destination-branch-absolute)))
	    (multiple-value-bind (merged-file conflict)
		(two-way-merge origin-file destination-file :conflicting t)
	      (when conflict
		(setf conflicts t)
		(format t "Conflict merging file ~S." file))
	      (list-to-file merged-file destination-file))))
	;; Create merge entry.
	(backend-get module access (list *access-file-name*)
		     destination-directory)
	(let ((access-file (merge-pathnames *access-file-name*
					    destination-directory)))
	  (multiple-value-bind (new-access accesses)
	      (include-access (make-instance 'access
					     :root (root origin-access))
			      access-file)
	    (let* ((origin (make-instance 'origin
					  :identifier origin-change
					  :access (identifier new-access)
					  :branch origin-branch))
		   (merge
		    (make-instance 'merge
				   :identifier (get-new-change-identifier
						destination-changes)
				   :file-map (create-new-file-map)
				   :origin origin)))
	      ;; Write out access file.
	      (write-access-file accesses
				 (merge-pathnames *access-file-name*
						  destination-directory))
	      ;; Add merge to new change-sequence
	      (setf new-changes (add-change merge new-changes)))))
	;; Add files to merge's file-map and write change sequence.
	(dolist (file common-files)
	  (setf new-changes (add-file-to-changes file new-changes)))
	(dolist (file uncommon-files)
	  (setf new-changes (add-file-to-changes file new-changes)))
	(write-change-file new-changes
			   (merge-pathnames *change-file-name*
					    destination-branch-absolute))
	;; Finish the operation.
	(if conflicts
	    ;; If there were conflicts during the merge (which will be
	    ;; the usual) the change sequence is saved and the
	    ;; temporary-directory is kept and its name returned.
	    ;; At this point it is important, that the
	    ;; in-temporary-directory macro does not delete the
	    ;; temporary-directory on the non-local exit
	    ;; (cf. always-cleanup option).
	    (progn
	      (port-path:delete-directory-tree origin-directory)
	      (format t "There were conflicts which have to be resolved.")
	      (format t "The conflicting files are in ~S."
		      (namestring destination-directory))
	      (return-from merge
		(values temporary-directory
			(union common-files uncommon-files :test #'equal))))
	    (let ((uncommon-files-prefixed (branch-prefix-file-list
					    uncommon-files
					    destination-branch-directory))
		  (common-files-prefixed (branch-prefix-file-list
					  common-files
					  destination-branch-directory)))
	      (port-path:in-directory destination-directory
		(backend-commit module
				(log-message-merge (first new-changes))
				access
				(append uncommon-files-prefixed
					common-files-prefixed
					(list destination-change-file
					      *access-file-name*)))))))))
  ;; If the operation completes we return NIL for success
  ;; (because in the case of conflicts the temporary pathname is returned).
  nil)

(defun log-message-merge (merge)
  "Generate a log message for a merge."
  (format nil "Created the following merge.~%~A"
	  (log-message-format merge)))

(defun merge-finish (module branch access directory files)
  "Finishes a stuck merge. branch and access have to be the same
as for merge and directory and files are merge's return values.
The directory is deleted after"
  (let ((destination-directory (merge-pathnames *merge-destination*
						directory)))
    (port-path:in-directory destination-directory
      (let* ((branch-directory (branch-identifier-to-directory branch))
	     ;; Include change file into list.
	     (files-prefixed (branch-prefix-file-list
			      (cons *change-file-name* files)
			      branch-directory)))
	;; Include access file into list.
	(backend-commit module "merge-finish" access
			(cons *access-file-name* files-prefixed)))))
  (port-path:delete-directory-tree directory))
