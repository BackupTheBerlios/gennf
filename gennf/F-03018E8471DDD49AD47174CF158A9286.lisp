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
;; $Id: F-03018E8471DDD49AD47174CF158A9286.lisp,v 1.6 2006/02/12 19:55:07 florenz Exp $

;; This file contains routines to manipulate changes,
;; change files, and sequences of changes.
;; All the file-map handling is in this file too.
;; Perhaps, this should be moved to another file.

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

(defun read-change-file (&optional (file *change-file*))
  (mapcar #'(lambda (alist) (alist-to-change alist)) (read-file file)))

(defun write-change-file (changes &optional (file *change-file*))
  "Writes a list of changes into a change file, which is
*change-file* by default."
  (prin1-file file changes))

(defun create-new-change-file (&optional (file *change-file*))
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

(defgeneric add-file-to-changes (file store)
  (:documentation "Add a mapping for file in the latest change of the sequence,
i. e. to the head of the sequence. If file was already added to the head
of the sequence nothing will change."))

(defmethod add-file-to-changes (file (sequence list))
  "Returns the new sequence of changes with file added
to the latest change."
  (ensure-string-pathname file)
  (let ((head (first sequence))
	(tail (rest sequence)))
    (if (lookup-in-file-map file (file-map head))
	sequence
	(cons (add-file
	       file (1+ (count-file-occurence file tail)) head)
	      tail))))

(defmethod add-file-to-changes (file (change-file pathname))
  "Writes a new change-file with file recorded in the
latest change."
  (write-change-file (add-file-to-changes file (read-file change-file))))

(defgeneric get-new-change-identifier (store)
  (:documentation "Gets a new change identifier (a new number)."))

(defmethod get-new-change-identifier ((sequence list))
  "Returns a new change identifier which is length of
sequence incremented by one."
  (1+ (length sequence)))

(defmethod get-new-change-identifier ((file pathname))
  "Computes a new change identifier using changes in file."
  (get-new-change-identifier (read-file file)))

(defgeneric get-change (identifier store)
  (:documentation "Return the indicated change."))

(defmethod get-change (identifier (sequence list))
  "Return the indicated change from sequence."
  ;; Sequences of changes are in reversed order.
  (nth (- (length sequence) identifier) sequence))

(defmethod get-change (identifier (file pathname))
  "Read file and then return the indicated change."
  (get-change identifier (read-file file)))

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

(defgeneric extract-files-and-revisions (store &optional identifier)
  (:documentation "For a given change identifier extract all files that are
in the branch up to and including that change.
The result is a list of dotted pairs (filename . revision),
with revision being the latest possible one of, course.
If no change is given, the latest one is assumed."))

(defmethod extract-files-and-revisions ((sequence list) &optional identifier)
  "Extract the required list from the given sequence of changes."
  ;; Latest change is default.
  (unless identifier
    (setf identifier (length sequence)))
  ;; Throw away changes that have higher identifiers than
  ;; the requested one.
  (setf sequence (nthcdr (- (length sequence) identifier) sequence))
  ;; file-revisions will be the result.
  (let ((file-revisions ()))
    ;; The file-maps of all changes are united with precedence
    ;; for newer entries.
    (dolist (change sequence)
      (setf file-revisions (alist-union (file-map change)
					file-revisions :test #'string=)))
    file-revisions))

(defmethod extract-files-and-revisions ((file pathname) &optional change)
  "Extract the required list from file."
  (extract-files-and-revisions (read-change-file file) change))