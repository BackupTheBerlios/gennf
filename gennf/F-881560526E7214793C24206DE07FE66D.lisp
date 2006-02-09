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
;; $Id: F-881560526E7214793C24206DE07FE66D.lisp,v 1.1 2006/02/09 13:23:25 sigsegv Exp $

;; Description: Defines mapping data structure based on MCVS's mapping.
;;              

(in-package :gennf)



;; *map-file* from configuration.lisp is now used.
;; (DEFPARAMETER *map-file* #p"/tmp/mapping.test" "Mapping file in *meta-dir*") 

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
    :documentation "F-File name")
   (path
    :initform ""
    :initarg :path
    :accessor path)
   (target
    :initform ""
    :initarg :target
    :accessor target)
   (executable 
    :initform nil
    :initarg :executable
    :accessor executable
    :documentation "Wether the mapped file should be executable")
   (raw-plist 
    :initform nil
    :initarg :raw-plist
    :accessor raw-plist)))

;; reads mcvs mapping-file 
;; returns map-list
(defun read-map-file (&optional (file *map-file*))
  (convert-map-file-in (read-file file)))

;; taken von mcvs:mapping.lisp
(defun convert-map-file-in (raw-filemap)
  "Converts a Gennf's filemap as read from a file into its internal
representation---a list of mapping-entry structures."
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
		;; NOT YET SUPPORTED
		;; 		   (when (fourth item)
		;; 		     (mapping-entry-parse-plist entry))
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

;; writes map-lists to file
(defun write-map-file (map-list &optional (file *map-file*))
  (let ((converted-map-list (convert-map-list-out map-list))
	(*print-pretty* t)
	(*print-right-margin* 1))
    (prin1-file file converted-map-list)))

;; Taken from mcvs:mapping.lisp
(defun convert-map-list-out (map-list) 
  (flet ((map-fun (map) 
	   (with-slots (kind id path target executable raw-plist) map
	     (if executable
		 (setf (getf raw-plist :exec) t)
		 (remf raw-plist :exec))
	     (ccase kind
	       (:file (list kind id path))
	       (:symlink) (list kind id path target)))))
    (mapcar #'map-fun map-list)))

;; prints object *only*  human readable to stream
;; This is not the output format of MCVS!
(defmethod print-object ((map mapping) stream)
  "Converting mapping into a human readable format.
   This is not the format of mcvs." 
  (print-unreadable-object (map stream)
      (with-slots (kind id path executable raw-plist) map
        (format stream "(~%:KIND ~s~%:ID ~s~%:PATH ~s~%:EXEC ~s~%:RAW ~s)"
                kind id path executable raw-plist))))

;; Creates new mappings but with sane defaults
;; Not really usefull, but trains me..
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