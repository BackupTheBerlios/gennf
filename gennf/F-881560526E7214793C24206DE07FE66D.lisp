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
;; $Id: F-881560526E7214793C24206DE07FE66D.lisp,v 1.26 2006/04/03 17:28:29 florenz Exp $

;; Description: creates directory structure by using a map file.
;; The format and the idea is derived from Meta-CVS.


;; TODO: 
;; - structure generating
;; - structure syncing
;; - implementing usual gennf file interface
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

;; Meta-CVS uses defstruct!
(defclass mapping ()
  ((kind
    :initform :file
    :initarg  :kind
    :accessor kind
    :documentation "Type of the mapping.")
   (id
    :initform (error ":id must be specified.")
    :initarg :id
    :accessor id
    :documentation "F-file name.")
   (path
    :initform (error ":path must be specified.")
    :initarg :path
    :accessor path
    :documentation "Hardlink path.")
   (target
    :initform ""
    :initarg :target
    :accessor target
    :documentation "Symbolic link target.")
   (executable
    :initform nil
    :initarg :executable
    :accessor executable
    :documentation "Whether the mapped file is executable.")
   (raw-plist 
    :initform nil
    :initarg :raw-plist
    :accessor raw-plist
    :documentation "Userattributes in a plist."))
  (:documentation "A single mapping for a single source file."))


(define-condition malformed-map-file-error (error)
  ((text 
    :initarg :text
    :reader text)
   (reader-condition
    :initarg :reader-condition
    :reader reader-condition)
   (file
    :initarg :file
    :reader file)))


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
  "Convert a mapping to a list representation, it is not
exactly an alist."
  (with-slots (kind id path target executable raw-plist) map
    (if executable
	(setf (getf raw-plist :exec) t)
	(remf raw-plist :exec))
    (setf path (namestring path))
    (setf id (namestring id))
    (case kind
      ((:file) (list* kind id path
		      (if raw-plist (list raw-plist))))
      ((:symlink) (list* kind id path target
			 (if raw-plist (list raw-plist))))
      (otherwise (error "Unknown mapping entry type ~S." kind)))))

(defun alist-to-mapping (alist)
  "Convert a list into a mapping."
  (when (or (not (consp alist))
	    (not (and (keywordp (first alist))
		      (stringp (second alist)))))
    (error 'malformed-map-file-error :text "Map-file broken.~%Please repair."))
  (case (first alist)
    ((:file)
     (let ((entry (make-instance 'mapping
				 :kind :file
				 ;; Internally representing
				 ;; all paths as pathnames.
				 :id (pathname (second alist))
				 :path (pathname (third alist))
				 :raw-plist (fourth alist))))
       (when (fourth alist)
	 (mapping-entry-parse-plist entry))
       entry))
    ((:symlink)
     (when (not (third alist))
       (error "Bad map: symbolic link ~A has no target." (second alist)))
     (make-instance 'mapping  
		    :kind :symlink
		    :id (pathname (second alist))
		    :path (pathname (third alist))
		    :target (pathname (fourth alist))
		  :raw-plist (fifth alist)))
    (otherwise (error "Bad type keyword ~A in map." (first alist)))))

(defun read-map-file (&optional (file *map-file-name*))
  "Reads Meta-CVS mapping-file and returns list of mappings."
  (handler-case 
      (mapcar #'alist-to-mapping (read-file file))
    (end-of-file (c) (error 'malformed-map-file-error :text "MAP-File broken."
			    :file file :reader-condition c))))


(defun write-map-file (mapping-list &optional (file *map-file-name*))
  "Writes mapping-lists to file."
  (prin1-file file mapping-list))

(defmethod print-object ((map mapping) stream)
  "Printing the object readable as alist."
  (if *print-readably*
            (call-next-method)
      (prin1 (convert-to-alist map) stream)))

(defun create-new-map-file (&optional (file *map-file-name*))
  "Write an empty map file."
  (write-map-file '() file))

(defgeneric add-mapping (mapping store)
  (:documentation "Add a new mapping to store."))

(defmethod add-mapping (map (file pathname))
  "Add a mapping (converted) into file."
  (prepend-to-list-file file map))

(defmethod add-mapping  (map (sequence list))
  "Add a mapping to sequence."
  (cons map sequence))

(defgeneric get-mapping (id store)
  (:documentation "Return mapping from store."))

(defmethod get-mapping ((identifier string) (sequence list))
  "Search the list for pathname- and filename-strings of identifier and path."
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
  "Get mapping by pathname from a list."
  (get-mapping (namestring id) sequence))

(defmethod get-mapping (id (file pathname))
  "Get mapping by from a map file."
  (get-mapping id (read-map-file file)))

(defun translate-to-f-files (file-list map-file)
  "Translates a list of filenames -- e. g. specified
by the user on the command line -- into the corrsponding
f-file names.
FIXME: This should include resolving relative pathnames etc."
  (mapcar #'(lambda (file)
	      (pathname-name (id (get-mapping file map-file))))
	  file-list))

(defgeneric remove-mapping (mapping store)
  (:documentation "Removes a mapping/identifer from store."))

(defmethod remove-mapping ((map mapping) (sequence list))
  "Remove mapping from a list. A new list is returned."
  (remove map sequence :test #'equal-mapping))

(defmethod remove-mapping ((identifier string) (sequence list))
  "Remove indicated mapping from a list. A new list is returned."
  (let ((map (get-mapping identifier sequence)))
    (remove-mapping map sequence)))

(defmethod remove-mapping ((map mapping) (file pathname))
  "Remove mapping from file and write new file."
  (write-map-file (remove-mapping map (read-map-file file)) file))

(defmethod remove-mapping ((identifier string) (file pathname))
  "Remove indicated mapping from file and write new file."
  (let* ((sequence (read-map-file file))
	 (list nil)
	 (map (get-mapping identifier sequence)))
    (unwind-protect
	 (setf list (remove-mapping map sequence))
      (write-map-file list file))))

(defgeneric sync-mappings (store branch)
  (:documentation "syncs listings of mappings to disk"))

(defmethod sync-mappings ((sequence list) branch)
  "Syncs list of mappings to  branch  disk"
  ;; FIXME: Place some tests here.
  (mapcar #'(lambda (mapping)
	      (format t "***** Syncing: ~a -> ~a ~%"
		      (id mapping) (path mapping))
	      (sync mapping branch))
	  sequence))

(defmethod sync-mappings ((file pathname) branch)
  "Syncs map-file in branch to disk"
  (sync-mappings (read-map-file file) branch))

(defun create-new-mapping (&key
			   (kind :file)
			   (id nil)
			   (path nil)
			   (target nil)
			   (executable nil)		
			   (raw-plist nil))
  "Wraps make-instance for consistency checks."
  (unless (and path id)
    (error "Must specify :path and :id."))
  (if (eql kind :symlink)
      (if (not target)
	  (error ":kind is symlink. Specify :target.")))
  (case kind
    (:file (make-instance 'mapping
			  :kind :file
			  :id id
			  :path path
			  :target target
			  :executable executable
			  :raw-plist raw-plist))
    (:symlink (make-instance 'mapping
			     :kind :symlink
			     :id id
			     :path path
			     :target target
			     :executable executable
			     :raw-plist raw-plist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defgeneric equal-mapping (left right)
  (:documentation "Compares two mapping concerning equality."))

(defmethod equal-mapping ((left mapping) (right mapping)) 
  ;; Taken from Meta-CVS.
  "Compares on following attributes:
kind id path target
path have to be in the same form directory."
  (and (eq (kind left) (kind right))
       (equal (id left) (id right))
       (equal (path left) (path right))
       (equal (target left) (target right))))

(defun equal-filemaps (left right)
  ;; Taken from Meta-CVS.
  "Compares two filemapes on equality."
  (let ((same t))
    (mapc #'(lambda (le re)
	      (setf same (and same (equal-mapping le re))))
	    left right)
    same))

(defun mapping-extract-kind (filemap kind)
  ;; Taken from Meta-CVS.
  "Extracts all kinds of mapping-list."
  (remove-if-not #'(lambda (entry-kind) 
		     (eq entry-kind kind))
		 filemap
		 :key #'kind))

(declaim (inline mapping-extract-paths))

(defun mapping-extract-paths (filemap)
  ;; Taken from Meta-CVS.
  "Return all paths from a list of mapping."
  (mapcar #'path filemap))

(defun mapping-lookup (filemap path)
  ;; Taken from Meta-CVS.
  "Return the indicated file from a list of mappings."
  (find path filemap :test #'equal :key #'path))

(defun mapping-prefix-lookup (filemap prefix)
  ;; Taken from Meta-CVS.
  ;; Meta-CVS uses *this-dir* for ".".
  "Return first mapping having prefix in its path."
  (if (equal #p"." prefix)
    (first filemap)
    (find prefix filemap :test #'equal :key #'path)))

(defun mapping-prefix-matches (filemap path)
  ;; Taken from Meta-CVS.
  "Return all mappings having prefix in their paths."
  (if (equal #p"." path)
    filemap
    (remove-if-not #'(lambda (entry) 
		       (port-path:pathname-prefix-p path (path entry))) 
		   filemap)))

(defun mapping-same-id-p (entry-one entry-two)
  ;; Taken from Meta-CVS.
  "Return if mapping have the same f-file name."
  (string= (id entry-one) (id entry-two)))

(defun mapping-same-path-p (entry-one entry-two)
  ;; Taken from Meta-CVS.
  "Return if paths are the same in both mappings."
  (string= (path entry-one) (path entry-two)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; FIXME: Not yet used in current code.
(defun mapping-dupe-check (filemap)
  ;; Taken from mcvs:mapping.lisp.
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



(defgeneric sync (mapping branch-directory)
  (:documentation "Syncs files from *meta-directory* to sandbox."))

(defmethod sync ((map mapping) branch-directory)
  "Synchronize from meta directory to sandbox."
  (port-path:in-directory *sandbox-directory*
    (with-slots (kind id path) map
      (let* ((f-file (port-path:append-pathnames *meta-directory*
						 branch-directory
						 id))
	     (absolute-path (merge-pathnames path))
	     (upper-dirs (port-path:pathname-prefixes absolute-path))
	     (not-existing-paths
	      (delete-if #'port-path:path-exists-p upper-dirs)))
	(mapcar #'port-path:create-directory not-existing-paths)
	(ecase kind
	  ;; Hardlinking
	  (:file
	   (if (and
		(port-path:path-exists-p f-file)
		(port-path:path-exists-p absolute-path))
	       ()			;; FIXME: compare for inode#.
	       (make-hard-link absolute-path f-file)))
	  ;; FIXME: Softlinking 
	  (:symlink 
	   (error "Symblic links are not yet implemented.")))))))

(defun make-hard-link (path1 path2)
  "Creates a hardlink for the missing pathspec.
path1 and path2 must be abolute."
  (let (existing
	to-create
	(exists1p (port-path:path-exists-p path1))
	(exists2p (port-path:path-exists-p path2)))
    (cond 
      ((and exists1p (not exists2p))
       (setf to-create path2)
       (setf existing path1))
      ((and exists2p (not exists1p))
       (setf to-create path1)
       (setf existing path2))
      ((and (not exists1p) (not exists2p))
       (error "None of the files exists."))
      (t (error "Both files exist.")))
    (port-path:hard-link existing to-create)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gerating ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Taken from Meta-CVS 1.1.0.
(defvar *have-dev-random* t)
(defvar *mcvs-random-state*)

(defun guid-gen ()
  "Generate random number, a unique identification for a new file."
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
