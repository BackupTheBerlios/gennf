(defpackage :gennf-commit)

(in-package :gennf-commit)

(defparameter *commit-number* 0
  "The counter for unique commit numbers.")

(defparameter *branch-number* 0
  "The counter for unique branch numbers.")

(defclass revision-number ()
  ((revision
    :accessor revision
    :documentation "The sequence of numbers denoting the CVS revision number."))
  (:documentation "CVS revision number. Normally written as rX.Y.Z."))

(defgeneric revision-number->string (revision)
  (:documentation "Converts a revision-number to string."))

(defmethod revision-number->string ((revision revision-number))
  (map 'string 

(defclass branch-identifier ()
  ((branch
    :accessor branch
    :documentation "Number of this branch"))
  (:documentation "Identification of branches"))


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
    :initform 0
    :documentation "Identifier of branch this commit belongs to")
   (access-identifier
    :accessor access-identifier
    :initarg :access-identifier
    :initform (make-instance 'access-identifier)
    :documentation "Identification of access method for this commit"))
  (:documentation "Class of a single commit."))


(defclass change (commit)
  ((file-revision-map
    :accessor file-revision-map
    :initform (make-hash-table)
    :initarg :file-revision-map
    :documentation "Maps filenames to revision-numbers."))
  (:documentation "A single change. This is the most common building block."))


(defclass link (commit)
  ()
  (:documentation "A link to another commit. This is needed for branching and inheritance."))


(defclass merge (change)
  ((origin
    :accessor origin
    :initform (make-hash-table)
    :initarg :origin
    :documentation "Link to the origin of the commit."))
  (:documentation "A merge is the result of applying the change origin to the current branch."))


(defgeneric add-file-to-commit (commit file revision)
  (:documentation "Add file to map of a commit"))

(defmethod add-file-to-commit ((commit change) file revision)
  (setf (gethash file (file-revision-map commit)) revision))


(defgeneric create-link-from-commit (commit)
  (:documentation "This creates a link to commit."))

(defmethod create-link-from-commit ((commit commit))
  (make-instance 'link
		 :commit-number (commit-number commit)
		 :branch-identifier (branch-identifier commit)
		 :access-identifier (access-identifier commit)))


(defgeneric create-merge-from-commit (commit)
  (:documentation "This generic function creates a merge which origin is commit."))

(defmethod create-merge-from-commit ((commit commit)
				      branch-identifier
				      access-identifier
				      file-revision-map)
  (make-instance 'merge
		 :branch-identifier branch-identifier
		 :access-identifier access-identifier
		 :file-revision-map file-revision-map
		 :origin commit))