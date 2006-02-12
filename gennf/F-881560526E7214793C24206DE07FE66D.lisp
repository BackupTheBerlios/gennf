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
;; $Id: F-881560526E7214793C24206DE07FE66D.lisp,v 1.4 2006/02/11 21:20:16 florenz Exp $

;; Description: creates directory structure by using a map file.
;; The format and the idea is derived from MCVS.


;; TODO: 
;; - interface
;;   - adding file
;;   - removing file
;;   - ... 
;; - generating Universal ID
;; - dublicate checking
;; - structure generating
;; - structure syncing
;; 
;; Done:
;; - reading
;; - writing

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

(defun read-map-file (&optional (file *map-file*))
  "Reads mcvs mapping-file and returns map-list."
  (convert-map-file-in (read-file file)))

(defun write-map-file (map-list &optional (file *map-file*))
  "Writes map-lists to file."
  (let ((converted-map-list (convert-map-list-out map-list))
	(*print-pretty* t)
	(*print-right-margin* 1))
    (prin1-file file converted-map-list)))

;; Taken from mcvs:mapping.lisp
(defun convert-map-file-in (raw-filemap)
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
  "Sets exec attribut on entry if raw-plist has exec prop."
  (with-slots (executable raw-plist) entry
    (destructuring-bind (&key exec &allow-other-keys) 
	raw-plist
      (setf executable exec)))
  (values))

;; Taken from mcvs:mapping.lisp
(defun convert-map-list-out (map-list) 
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

;; Prints object *only* human readable to stream
;; This is not the output format of MCVS!
(defmethod print-object ((map mapping) stream)
  "Converting mapping into a human readable format.
This is not the format of mcvs." 
  (print-unreadable-object (map stream)
      (with-slots (kind id path executable raw-plist) map
        (format stream "(~%:KIND ~s~%:ID ~s~%:PATH ~s~%:EXEC ~s~%:RAW ~s)"
                kind id path executable raw-plist))))

;; Creates new mappings but with sane defaults
;; Not really useful, but trains me..
;; Whats the different between (make-instance )
(defun make-new-mapping (&key (kind :file) (id "meine erste ID") (path nil path-set-p)
                         (executable nil) (raw-plist nil))
  (when (not path-set-p)
    (error "Path has to be specifyed."))
  (make-instance 'mapping
                 :kind kind :id id
                 :path path :executable executable
                 :raw-plist raw-plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example data 
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