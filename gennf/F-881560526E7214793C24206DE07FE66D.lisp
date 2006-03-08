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
;; $Id: F-881560526E7214793C24206DE07FE66D.lisp,v 1.10 2006/03/08 14:35:35 sigsegv Exp $

;; Description: creates directory structure by using a map file.
;; The format and the idea is derived from MCVS.


;; TODO: 
;; - structure generating
;; - structure syncing
;; 
;; Done:
;; - reading
;; - writing
;; - generating Universal ID
;; - dublicate checking
;; 
;; Not needed:
;; - interface
;;   - adding file
;;   - removing file


(in-package :gennf)

;; MCVS uses defstruct!
(defclass mapping ()
  ((kind
    :initform :file
    :initarg  :kind
    :accessor kind
    :documentation "Type of the mapping")
   (id
    :initform ""
    :initarg :id
    :accessor id
    :documentation "F-file name")
   (path
    :initform ""
    :initarg :path
    :accessor path)
   (target
    :initform ""
    :initarg :target
    :accessor target
    :documentation "Symbolic link target")
   (executable 
    :initform nil
    :initarg :executable
    :accessor executable
    :documentation "Wether the mapped file should be executable")
   (raw-plist 
    :initform nil
    :initarg :raw-plist
    :accessor raw-plist
    :documentation "Userattributes in a plist")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading and Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defun read-map-file (&optional (file *map-file*))
  "Reads mcvs mapping-file and returns list of mapings."
  (convert-map-file-in (read-file file)))

(defun write-map-file (map-list &optional (file *map-file*))
  "Writes map-lists to file."
  (let ((converted-map-list (convert-map-list-out map-list))
	(*print-pretty* t)
	(*print-right-margin* 1))
    (prin1-file file converted-map-list)))


(defun convert-map-file-in (raw-filemap)
;; Taken from mcvs:mapping.lisp
  "Converts a gennf filemap as read from a file into its internal
representation -- a list of mapping-entry structures."
  (flet ((map-fun (item)
	   (when (or (not (consp item))
		     (not (and (keywordp (first item))
			       (stringp (second item)))))
	     (error "map-file broken"))
	   (case (first item)
	     ((:file)
	      (let ((entry (make-instance 'mapping
					  :kind :file
					  :id (second item)
					  :path (third item)
					  :raw-plist (fourth item))))
		(when (fourth item)
		  (mapping-entry-parse-plist entry))
		entry))
	     ((:symlink)
	      (when (not (third item))
		(error "bad map: symlink ~a has no target."
		       (second item)))
	      (make-instance 'mapping  
			     :kind :symlink
			     :id (second item)
			     :path (third item)
			     :target (fourth item)
			     :raw-plist (fifth item)))
	     (otherwise (error "bad type keyword ~s in map." 
			       (first item))))))
    (mapcar #'map-fun raw-filemap)))

(defun mapping-entry-parse-plist (entry)
;; Taken from mcvs:mapping.lisp
  "Sets exec attribut on entry if raw-plist has exec prop."
  (with-slots (executable raw-plist) entry
    (destructuring-bind (&key exec &allow-other-keys) 
	raw-plist
      (setf executable exec)))
  (values))


(defun convert-map-list-out (map-list) 
;; Taken from mcvs:mapping.lisp
  (flet ((map-fun (map) 
	   (with-slots (kind id path target executable raw-plist) map
	     (if executable
		 (setf (getf raw-plist :exec) t)
		 (remf raw-plist :exec))
	     (ccase kind
	       (:file (list kind id path
			    (if raw-plist 
				(list raw-plist))))
	       (:symlink (list kind id path target
				(if raw-plist (list raw-plist))))))))
	 (mapcar #'map-fun map-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defgeneric equal-mapping (left right)
  (:documentation "Compares two mapping concernig equality"))


(defmethod equal-mapping ((left mapping) (right mapping)) 
  ;; taken from mcvs.
  "Compares on following attributes
\(kind id path target\)
path have to bee in the same form (directory) "
  (and (eq (kind left) (kind right))
       (string= (id left) (id right))
       (equal (path left) (path right))
       (equal (target left) (target right))))

(defun equal-filemaps (left right)
  ;; taken from mcvs
"Compares two filemapes on equality."
  (let ((same t))
    (mapc #'(lambda (le re)
	      (setf same (and same (equal-mapping-entries le re))))
	    left right)
    same))

(defun mapping-extract-kind (filemap kind)
  ;; taken from mcvs
 "extracts all kinds of mapping-list"
  (remove-if-not #'(lambda (entry-kind) 
		     (eq entry-kind kind))
		 filemap
		 :key #'kind))

(declaim (inline mapping-extract-paths))
(defun mapping-extract-paths (filemap)
  ;; taken from mcvs
  (mapcar #'path filemap))

(defun mapping-lookup (filemap path)
  ;; taken from mcvs
  (find path filemap :test #'equal :key #'path))

(defun mapping-prefix-lookup (filemap prefix)
  ;; taken from mcvs
  ;; mcvs uses *this-dir* for "."
  (if (equal #p"." prefix)
    (first filemap)
    (find prefix filemap :test #'equal :key #'path)))

(defun mapping-prefix-matches (filemap path)
  ;; taken from mcvs
  ;; (if (equal *this-dir* path)		
  (if (equal #p"." path)
    filemap
    (remove-if-not #'(lambda (entry) 
		       (port-path:pathname-prefix-p path (path entry))) 
		   filemap)))

(defun mapping-same-id-p (entry-one entry-two)
  ;; taken from mcvs
  (string= (id entry-one) (id entry-two)))

(defun mapping-same-path-p (entry-one entry-two)
  ;; taken from mcvs
  (equal (path entry-one) (path entry-two)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; FIXME: Not yet used in existing code.
(defun mapping-dupe-check (filemap)
;; Taken from mcvs:mapping.lisp
"Signals an error condition if the filemap contains duplicate paths or
duplicate objects. Otherwise returns the filemap, sorted by path."
  (let ((dupes)
	(id-hash (make-hash-table :test #'equal))
	(path-hash (make-hash-table :test #'equal)))
    (dolist (entry filemap)
      (if (gethash (id entry) id-hash)
	(push entry dupes)
	(setf (gethash (id entry) id-hash) entry))
      (if (gethash (path entry) path-hash)
	(push entry dupes)
	(setf (gethash (path entry) path-hash) entry)))
    (when dupes
      (dolist (dupe dupes)
	;; 	(chatter-terse "duplicate ~a -> ~a~%" 
	;; 		       (id dupe) (path dupe)))
	;; FIXME: Use more abstraction for format
	;; so that a common debug exists.
	(format t "duplicate ~a -> ~a~%" 
		       (id dupe) (path dupe)))

      (error "duplicates in map: correct and run mcvs update.")))
  filemap)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syncing 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sync-files ()

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gerating ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Taken from MCVS 1.1.0
(defvar *have-dev-random* t)
(defvar *mcvs-random-state*)

(defun guid-gen ()
  (cond
    (*have-dev-random*
       (or (ignore-errors 
	     (with-open-file (f "/dev/urandom" 
				:direction :input 
				:element-type '(unsigned-byte 128))
	       (read-byte f)))
	   (progn
	     (setf *have-dev-random* nil)
	     (setf *mcvs-random-state* (make-random-state t))
	     (guid-gen))))
    (t (random #.(expt 2 128) *mcvs-random-state*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debuggin and Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-data* nil)

(push (make-instance 'mapping
		     :path #p"a/simple/path/which/does/not/exist/"
		     :executable 't)
      *test-data*)

(push (make-instance 'mapping 
		     :path #p"/an/absolute/path/w/d/n/e/"
		     :executable 'nil)
      *test-data*)


;; Prints object *only* human readable to stream
;; This is not the output format of MCVS!
(defmethod print-object ((map mapping) stream)
  "Converting mapping into a human readable format.
This is not the format of mcvs." 
  (print-unreadable-object (map stream)
      (with-slots (kind id path executable raw-plist) map
        (format stream "(~%:KIND ~s~%:ID ~s~%:PATH ~s~%:EXEC ~s~%:RAW ~s)"
                kind id path executable raw-plist))))




