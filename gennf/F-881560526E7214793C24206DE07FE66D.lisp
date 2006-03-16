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
;; $Id: F-881560526E7214793C24206DE07FE66D.lisp,v 1.17 2006/03/16 11:44:57 sigsegv Exp $

;; Description: creates directory structure by using a map file.
;; The format and the idea is derived from MCVS.


;; TODO: 
;; - structure generating
;; - structure syncing
;; - implementing usal gennf file interface
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
    :initform (error ":id must be specified")
    :initarg :id
    :accessor id
    :documentation "F-file name")
   (path
    :initform (error ":path must be specified")
    :initarg :path
    :accessor path
    :documentation "Hardlink path")
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
;; Operations (Reading, Writing, Adding)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(defun mapping-entry-parse-plist (entry)
;; Taken from mcvs:mapping.lisp
  "Sets exec attribut on entry if raw-plist has exec prop."
  (with-slots (executable raw-plist) entry
    (destructuring-bind (&key exec &allow-other-keys) 
	raw-plist
      (setf executable exec)))
  (values))

(defmethod convert-to-alist append ((map mapping))
  (with-slots (kind id path target executable raw-plist) map
    (if executable
	(setf (getf raw-plist :exec) t)
	(remf raw-plist :exec))
    (case kind
      ((:FILE) (list* kind id path 
		      (if raw-plist (list raw-PLIST))))
      ((:SYMLINK) (list* kind id path target
			 (if raw-plist (list raw-plist))))
      (otherwise (error "unknown mapping entry type ~s." kind)))))

(defun alist-to-mapping (alist)
  "Convert a list into a mapping"
  (when (or (not (consp alist))
	    (not (and (keywordp (first alist))
		      (stringp (second alist)))))
    (error "map-file broken"))
  (case (first alist)
    ((:FILE)
     (let ((entry (make-instance 'mapping
				 :kind :FILE
				 ;; internaly representing
				 ;; all paths as pathnames
				 :id (pathname (second alist))
				 :path (pathname (third alist))
				 :raw-plist (fourth alist))))
       (when (fourth alist)
	 (mapping-entry-parse-plist entry))
       entry))
    ((:SYMLINK)
     (when (not (third alist))
       (error "bad map: symlink ~a has no target."
	      (second alist)))
     (make-instance 'mapping  
		    :kind :SYMLINK
		    :id (pathname (second alist))
		    :path (pathname (third alist))
		    :target (pathname (fourth alist))
		  :raw-plist (fifth alist)))
    (otherwise (error "bad type keyword ~s in map." 
		      (first alist)))))

(defun read-map-file (&optional (file *map-file-name*))
  "Reads mcvs mapping-file and returns list of mapings."
  (mapcar #'alist-to-mapping (read-file file)))

(defun write-map-file (map-list &optional (file *map-file-name*))
  "Writes map-lists to file."
  (prin1-file file map-list))

(defmethod print-object ((map mapping) stream)
  "printing the obeject readable as alist"
  (if *print-readably*
            (call-next-method)
      (prin1 (convert-to-alist map) stream)))

(defun create-new-map-file (&optional (file *map-file-name*))
  (write-map-file '() file))

(defgeneric add-mapping (mapping store)
  (:documentation "Add a new mapping to store"))

(defmethod add-mapping (map (file pathname))
  "Add a mapping (converted) into file."
  (prepend-to-list-file file map))

(defmethod add-mapping  (map (sequence list))
  "add a mapping to sequence"
  (cons map sequence))

(defgeneric get-mapping (id store)
  (:documentation "Return mapping from store"))

(defmethod get-mapping ((identifier string) (sequence list))
  "Search the list for pathname- and filename-strings of
id and path"
  (find-if #'(lambda (x) 
	       (with-slots (id path) x
		 (let ((id-namestring (namestring id))
		       (id-filename (file-namestring id))
		       (path-namestring (namestring path))
		       (path-filename (file-namestring path)))
		   (or
		    (string= identifier id-namestring)
		    (string= identifier id-filename)
		    (string= identifier path-namestring)
		    (string= identifier path-filename)))))
	   sequence))

(defmethod get-mapping ((id pathname) (sequence list))
  (find id sequence :key #'id :test #'equal))

(defmethod get-mapping (id (file pathname))
  (get-mapping id (read-map-file file)))

(defgeneric remove-mapping (mapping store)
  (:documentation "Removes a mapping/identifyer from store"))

(defmethod remove-mapping ((map mapping) (sequence list))
  (remove map sequence :test #'equal-mapping))

(defmethod remove-mapping ((identifier string) (sequence list))
  (let ((map (get-mapping identifier sequence)))
    (remove-mapping map sequence)))

(defmethod remove-mapping ((map mapping) (file pathname))
    (remove-mapping map file))

(defmethod remove-mapping ((identifier string) (file pathname))
  (let* ((sequence (read-map-file file))
	 (list nil)
	 (map (get-mapping identifier sequence)))
    (unwind-protect
	 (setf list (remove-mapping map sequence))
      (write-map-file list file))))

;; this one checks for file existence etc. 
;; Redesign: dwim-function
(defun create-new-mapping (&key
			   (kind :file)
			   (id nil)
			   (path nil)
			   (target nil)
			   (executable nil)
			   (raw-plist nil))
  "Wraps make-instance for consintency checks and filetype detection.
   
  Make sure its evaluated in the right directory."
  (unless (and path id)
    (error "Must specify :path and :id"))
  (unless (port-path:path-exists-p path)
    (error ":path, must exist. Changed path?"))
  (if (eql kind :SYMLINK)
      (if (not target)
	  (error ":kind is symlink. Specify :target")))
  (flet ((filetype (file)
	   (case (osicat:file-kind file)
	     (:REGULAR-FILE :FILE)
	     (:SYMBOLIC-LINK :SYMLINK))))
    (case (filetype path)
      (:FILE (make-instance 'mapping 
			    :kind :FILE
			    :id id
			    :path path
			    :target target
			    :executable executable
			    :raw-plist raw-plist))
      (:SYMLINK (make-instance 'mapping
			       :kind :SYMLINK
			       :id id
			       :path path
			       :target target
			       :executable executable
			       :raw-plist raw-plist)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defgeneric equal-mapping (left right)
  (:documentation "Compares two mapping concernig equality"))

(defmethod equal-mapping ((left mapping) (right mapping)) 
  ;; taken from mcvs.
  "Compares on following attributes
kind id path target
path have to be in the same form directory"
  
  (and (eq (kind left) (kind right))
       (equal (id left) (id right))
       (equal (path left) (path right))
       (equal (target left) (target right))))

(defun equal-filemaps (left right)
  ;; taken from mcvs
"Compares two filemapes on equality."
  (let ((same t))
    (mapc #'(lambda (le re)
	      (setf same (and same (equal-mapping le re))))
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

(defun make-hard-link (path1 path2)
  "Creates a hardlink for the missing pathspec.
Path1 and Path2 must be abolute."
  (let (existend to-create)
    (cond 
      ((port-path:path-exists-p path1)
       (setf to-create path2)
       (setf existend path1))
      ((port-path:path-exists-p path2)
       (setf to-create path1)
       (setf existend path2))
      (t (error "Both files exist.")))
    ;; pretty strange but....
    (osicat:make-link to-create :target existend :hard t)))

(defgeneric sync (mapping) 
  (:documentation "syncs files from *meta-directory* to sandbox"))

(defmethod sync ((map mapping))
  (port-path:in-directory (port-path:get-parent-directory *meta-directory*)
    (with-slots (kind id path) map
      (let* ((absolute-path (merge-pathnames path))
	    (upper-dirs (port-path:pathname-prefixes absolute-path))
	    (not-existing-pathes
	     (delete-if #'port-path:path-exists-p upper-dirs)))
	(mapcar #'port-path:create-directory not-existing-pathes)
	(ecase kind
	  ;; Hardlinking
	  (:FILE (make-hard-link absolute-path (merge-pathnames id)))
	  ;; FIXME: Softlinking 
	  (:SYMLINK 
	   (error "SYMLINKS are NOT yet Implemented")))))))


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

;; (defparameter *test-data* nil)

;; (push (make-instance 'mapping
;; 		     :path #p"a/simple/path/which/does/not/exist/"
;; 		     :executable 't)
;;       *test-data*)

;; (push (make-instance 'mapping 
;; 		     :path #p"/an/absolute/path/w/d/n/e/"
;; 		     :executable 'nil)
;;       *test-data*)





