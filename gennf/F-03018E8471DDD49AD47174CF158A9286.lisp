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
;; $Id: F-03018E8471DDD49AD47174CF158A9286.lisp,v 1.22 2006/04/01 12:45:52 florenz Exp $

;; This file contains routines to manipulate changes,
;; change files, and sequences of changes.
;; All the file-map handling is in this file too.

(in-package :gennf)

(defclass change ()
  ((identifier :initarg :identifier
	       :accessor identifier
	       :documentation "A unique natural number.")
   (file-map :initarg :file-map
	     :accessor file-map
	     :documentation "A map filename to revision number
implemented as an alist."))
  (:documentation "A change is the atomic unit of a repository."))

(defun is-change-p (alist)
  "Test if given alist is a change representation."
  (and (= (length alist) 2)
       (eql (car (first alist)) :identifier)
       (eql (car (second alist)) :file-map)))

(defmethod convert-to-alist append ((change change))
  "Converts a change to an alist. First entry is identifier,
second is the file-map."
  (acons :identifier (identifier change)
	 (acons :file-map (file-map change) ())))

(defun alist-to-change (alist)
  "Converts a proper alist to a change object."
  (when (is-change-p alist)
    (make-instance 'change
		   :identifier (extract :identifier alist)
		   :file-map (extract :file-map alist))))
		   
(defmethod print-object ((change change) stream)
  "Prints a change as an alist, only if *print-readably* is
NIL. This is due to the language standard which says:
if *print-readably* is T, reading of a print-object printed
object yields a similair object with standard readtable in
action. As this can not be accomplished with converting
an object to a list, which can be read by the reader but
as a list, this method passes if *print-radably* is T."
  (if *print-readably*
      (call-next-method)
      (prin1 (convert-to-alist change) stream)))

(defun read-change-file (&optional (file *change-file-name*))
  "Reads file as a change file. Each entry is checked if
it is a merge or a change and appropriate objects are created."
  (flet ((create-object (alist)
	   (cond ((is-change-p alist) (alist-to-change alist))
		 ((is-merge-p alist) (alist-to-merge alist))
		 (t (error "Garbage in the change file.")))))
    (mapcar #'create-object (read-file file))))

(defun write-change-file (changes &optional (file *change-file-name*))
  "Writes a list of changes into a change file, which is
*change-file-name* by default."
  (prin1-file file changes))

(defun create-new-change-file (&optional (file *change-file-name*))
  "Creates a new cange file containg an empty list."
  (write-change-file () file))

(defun create-new-file-map ()
  "Return an empty file-map."
  ())

(defun lookup-in-file-map (file file-map)
  "Returns the file's revision recorded in file-map."
  (cdr (assoc file file-map :test #'string=)))

(defun remove-from-file-map (file file-map)
  "Deletes file from file-map and return new file-map.
If file occurs several times -- which is an error -- all occurences
are removed."
  (remove-if #'(lambda (pair) (string= file (car pair))) file-map))

(defun add-to-file-map (file occurence file-map)
  "Add an entry file->occurence to file-map and return
the new file-map. If file is already recorded in file-map
no modification occurs (use update-file-map for this
purpose)."
  (if (lookup-in-file-map file file-map)
      file-map
      (acons file occurence file-map)))

(defun update-file-map (file occurence file-map)
  "Update file's record in file-map and return the new file-map.
If file is not recorded update-file-map is identity."
  (if (lookup-in-file-map file file-map)
      (mapcar #'(lambda (pair)
		  (if (string= (car pair) file)
		      (cons file occurence)
		      pair)) file-map)
      file-map))

(defgeneric add-file (file occurence change)
  (:documentation "Record file->occurence in change."))

(defmethod add-file (file occurence (change change))
  (exchange-file-map change
		     (add-to-file-map file occurence
				      (file-map change))))

(defgeneric exchange-file-map (change file-map)
  (:documentation "Exchange the file-map of some change."))

(defmethod exchange-file-map ((change change) file-map)
  "Replaces change's file-map by another one (destructive)."
  (setf (file-map change) file-map)
  change)

(defun count-file-occurence (file sequence)
  "Counts, how often file is recorded in the
file-maps of all changes in sequence."
  (let ((occurences 0))
    (dolist (change sequence)
      (when (lookup-in-file-map file (file-map change))
	(incf occurences)))
    occurences))

(defgeneric take-older-changes (changes &optional n)
  (:documentation "Takes the n oldest changes from the changes list."))

(defmethod take-older-changes ((sequence list) &optional (n 1))
  "Takes n last elements of the list."
  (nthcdr (- (length sequence) n) sequence))

(defmethod take-older-changes ((file pathname) &optional (n 1))
  "Takes n last records from file."
  (take-older-changes (read-change-file file) n))

(defgeneric all-changed-files (change &optional identifier)
  (:documentation "Returns all files that were
changes, without a their revision number, as a list of
files."))

(defmethod all-changed-files ((change change) &optional identifier)
  "Return all files recorded in change's filemap"
  (declare (ignore identifier)) ; It is not needed but the generic has it.
  (let ((file-map (file-map change)))
    (loop for file-revision in file-map 
	  collect (pathname (car file-revision)))))

(defmethod all-changed-files ((changes list) &optional identifier)
  "Return all files recorded in all changes in the list."
  (when identifier
    (setf changes (without-newer-changes identifier changes)))
  (let ((files ()))
    (dolist (change changes)
      (setf files (union files (all-changed-files change) :test #'equal)))
    files))

(defgeneric add-file-to-changes (file store)
  (:documentation "Add a mapping for file in the latest change of the sequence,
i. e. to the head of the sequence. If file was already added to the head
of the sequence nothing will change."))

(defmethod add-file-to-changes (file (sequence list))
  "Returns the new sequence of changes with file added
to the latest change."
  (port-path:ensure-string-pathname file)
  (let ((head (if (null sequence)
		  (make-instance
		   'change
		   :identifier (get-new-change-identifier sequence)
		   :file-map (create-new-file-map))
		  (first sequence)))
	(tail (rest sequence)))
    (if (lookup-in-file-map file (file-map head))
	sequence
	(cons (add-file
	       file (1+ (count-file-occurence file tail)) head)
	      tail))))

(defmethod add-file-to-changes (file (change-file pathname))
  "Writes a new change-file with file recorded in the
latest change."
  (write-change-file (add-file-to-changes file (read-file change-file))
		     change-file))

(defgeneric get-modified-files (older-changes newer-changes)
  (:documentation "Return a list of files, which
have been modified from latest change of older-changes to latest
change of newer-changes.
older-changes has to be a tail of newer-changes, i. e. in fact
an older version of newer-changes."))

(defmethod get-modified-files ((older-changes list) (newer-changes list))
  "Extract modified files from lists."
  (let ((new-changes (butlast newer-changes (length older-changes)))
	(files ()))
    (dolist (change new-changes)
      (setf files (union files (all-changed-files change) :test #'equal)))
    (mapcar #'pathname files)))

(defmethod get-modified-files ((older-changes pathname)
			       (newer-changes pathname))
  "Extract modified files from change files."
  (get-modified-files (read-change-file older-changes)
		      (read-change-file newer-changes)))

(defgeneric get-new-change-identifier (store)
  (:documentation "Gets a new change identifier (a new number)."))

(defmethod get-new-change-identifier ((sequence list))
  "Returns a new change identifier which is length of
sequence incremented by one."
  (1+ (length sequence)))

(defmethod get-new-change-identifier ((file pathname))
  "Computes a new change identifier using changes in file."
  (get-new-change-identifier (read-file file)))

(defgeneric get-change (store &optional identifier)
  (:documentation "Return the indicated change, latest if no identifier
is given."))

(defmethod get-change ((sequence list)
		       &optional (identifier (length sequence)))
  "Return the indicated change from sequence."
  ;; Sequences of changes are in reversed order.
  (nth (- (length sequence) identifier) sequence))

(defmethod get-change ((file pathname) &optional identifier)
  "Read file and then return the indicated change."
  (let ((changes (read-change-file file)))
    (get-change changes (if identifier identifier (length changes)))))

(defgeneric add-change (change store)
  (:documentation "Add a new change to the given store. The
added change become shead of the change sequence."))

(defmethod add-change (change (sequence list))
  "Prepend change to sequence."
  (cons change sequence))

(defmethod add-change (change (file pathname))
  "Write a new change file containing all changes of
file plus a new one being head of the sequence."
  (prepend-to-list-file file change))

(defgeneric without-newer-changes (identifier changes)
  (:documentation "Remove all changes newer than identifer and return
the remaining sequence."))

(defmethod without-newer-changes (identifier (sequence list))
  "Read changes form sequence."
  (nthcdr (- (length sequence) identifier) sequence))

(defmethod without-newer-changes (identifier (file pathname))
  "Read changes from file."
  (without-newer-changes identifier (read-change-file file)))
  
(defgeneric extract-files-and-revisions (store &key identifier files)
  (:documentation "For a given change identifier extract all files that are
in the branch up to and including that change.
The result is a list of dotted pairs (filename . revision),
with revision being the latest possible one of, course.
If no change is given, the latest one is assumed.
If the key argument files is supplied, only those files
appear in the result that are mentioned in this file list."))

(defmethod extract-files-and-revisions ((sequence list) &key identifier
					(files () files-supplied-p))
  "Extract the required list from the given sequence of changes."
  ;; Latest change is default.
  (unless identifier
    (setf identifier (length sequence)))
  ;; Throw away changes that have higher identifiers than
  ;; the requested one.
  (setf sequence (without-newer-changes identifier sequence))
  ;; file-revisions will be the result.
  (let ((file-revisions ()))
    ;; The file-maps of all changes are united with precedence
    ;; for newer entries.
    (dolist (change sequence)
      (setf file-revisions (alist-union (file-map change)
					file-revisions :test #'string=)))

    ;; Translate filenames to pathnames.
    (setf file-revisions
	  (mapcar #'(lambda (pair)
		      (cons (pathname (car pair)) (cdr pair)))
		  file-revisions))
    ;; If only certain files are demanded, all others are filtered out.
    (if files-supplied-p
	(remove-if-not #'(lambda (pair)
			   (member (car pair) files :test #'equal))
		       file-revisions)
	file-revisions)))

(defmethod extract-files-and-revisions ((file pathname) &key identifier
					(files () files-supplied-p))
  "Extract the required list from file."
  (if files-supplied-p
      (extract-files-and-revisions (read-change-file file)
				   :identifier identifier :files files)
      (extract-files-and-revisions (read-change-file file)
				   :identifier identifier)))

(defun remove-revisions (alist)
  "Removes the revisions from a file-revision list as produced
by extract-files-and-revisions."
  (mapcar #'(lambda (pair) (car pair)) alist))

(defun retrieve-latest-changes (module access branch)
  "Returns latest change sequence for the indicated module
directly from the repository."
  (let ((changes ())
	(change-file
	 (merge-pathnames *change-file-name*
			  (branch-identifier-to-directory branch))))
    (port-path:in-temporary-directory (:temporary-pathname temporary-directory)
      (backend-get module access (list change-file) temporary-directory)
      (setf changes (read-change-file change-file)))
    changes))

(defmethod log-message-format ((change change))
  "Returns a string suitable to be put into a log message
containg the change number and all changed files."
  (format nil "Change-identifier ~A.~%Changed files: ~A."
	  (identifier change)
	  (list-to-string (all-changed-files change) :convert #'namestring)))

(defun retrieve-all-changes (module access branch &optional change)
  "Returns a list of all changes in sequence plus
those that are linked by the oldest change in the sequence."
  (let ((start (let ((sequence (retrieve-latest-changes module access branch)))
		 (if change
		     (without-newer-changes change sequence)
		     sequence))))
    (debug
      (debug-format "~A~%" start)
      (debug-format "Second type is ~A~%." (type-of (second (reverse start)))))
    ;; Grab out the second change in the sequence. If this is a merge
    ;; its origin is followed to get the other changes. It is the second
    ;; cange because the first is always the one with the map file.
    (let ((possibly-merge (second (reverse start))))
      (if (typep possibly-merge 'merge)
	  (let* ((origin (origin possibly-merge))
		 (latest-access (retrieve-latest-access module access))
		 (remote-branch (branch origin))
		 (remote-access (get-access (access origin) latest-access))
		 (remote-change (identifier origin)))
	    (append start (retrieve-all-changes module remote-access
						remote-branch remote-change)))
	  start))))

(defmethod info-format ((change change))
  "Return <CHANGE: number>."
  (format nil "<CHANGE: ~A>" (identifier change)))
