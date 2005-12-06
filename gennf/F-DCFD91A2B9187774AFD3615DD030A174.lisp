(defpackage :gennf-commit)

(in-package :gennf-commit)

(defparameter *commit-number* 0)

(defclass revision-number ()
  ((revision
    :accessor revision
    :documentation "The sequence of numbers denoting the CVS revision number."))
  (:documentation "CVS revision number. Normally written as rX.Y.Z."))

;(defclass branch-identifier ()
;  ((branch
;    :accessor branch
;    :documentation "Number of this branch"))
;  (:documentation "Identification of branches"))

(defclass access-identifier ()
  ((host-identifier
    :accessor host-identifier
    :documentation "Fully qualified host-name")
   (acces-method
    :accessor access-method
    :documentation "How to communicate with the remote host, e. g. ssh, filesystem.")
   (path
    :accessor path
    :documentation "Path of the remote repository using Lisp's pathname abstraction.")))

(defclass commit ()
  ((commit-number
    :accessor commit-number
    :initform (incf *commit-number*)
    :initarg :commit-number
    :documentation "Unique commit identifier")
   (branch-identifier
    :accessor branch-identifier
    :initarg :branch-identifier
    :documentation "Identifier of branch this commit belongs to")
   (access-identifier
    :accessor access-identifier
    :initarg :access-identifier
    :documentation "Identification of access method for this commit"))
  (:documentation "Class of a single commit."))

(defclass change (commit)
  ((file-revision-map
    :accessor file-revision-map
    :initform (make-hash-table)
    :documentation "Maps filenames to revision-numbers."))
  (:documentation "Class of a change."))

(defclass link (commit)
  ()
  (:documentation "Class of a link."))

(defclass merger (change)
  ((origin
    :accessor origin
    :initform (make-hash-table)
    :initarg :origin
    :documentation "Link to the origin of the commit."))
  (:documentation "Class of a merge."))


(defgeneric add-file-to-commit (commit file revision)
  (:documentation "Add file to map of a commit"))


(defmethod add-file-to-commit ((commit change) file revision)
  (setf (gethash file (file-revision-map commit)) revision))

(defmethod add-file-to-commit ((commit merger) file revision)
  (setf (gethash file (file-revision-map commit)) revision))


(defgeneric create-link-from-commit (commit)
 ( :documentation "This is ^c."))

;; bloede klassen? werma dama sehn
(defmethod create-link-from-commit ((commit commit))
  (make-instance 'link
		 :commit-number (commit-number change)
		 :branch-identifier (branch-identifier change)
		 :access-identifier (access-identifier change)))

(defmethod create-merger-from-commit ((commit commit)
				      branch-identifier access-identifier file-revision-map)
  (make-instance 'merger
		:branch-identifier branch-identifier
		:access-identifier access-identifier
		:file-revision-map file-revision-map
		:origin commit))