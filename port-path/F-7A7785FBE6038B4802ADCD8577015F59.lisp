;; Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
;;
;; This file is part of port-path.
;;
;; port-path is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; port-path is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gennf; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; $Id: F-7A7785FBE6038B4802ADCD8577015F59.lisp,v 1.16 2006/03/15 18:08:00 florenz Exp $

;; Implements all the main functionality of port-path
;; and some required helper functions.

(in-package :port-path)

(defmacro with-gensyms ((&rest names) &body forms)
  "Generate symbols for all names given to be used in
a macro. Taken from Peter Seibel's book, chapter 8."
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@forms))

(uffi:def-function ("port_path_tempnam" c-tempnam) ()
  :module "port-path"
  :returning (* :char))

(defmacro with-directory-form (variable-pathspec-pairs &body forms)
  "Assigns each variable the pathname in directory form denoted
by pathspec."
  `(let ,(mapcar
	  (lambda (pair)
	    `(,(first pair) (pathname-to-directory-form ,(second pair))))
	  variable-pathspec-pairs)
    ,@forms))

(defmacro with-file-form (variable-pathspec-pairs &body forms)
  "Assigns each variable the pathname in file form denoted
by pathspec."
  `(let ,(mapcar
	  (lambda (pair)
	    `(,(first pair) (pathname-to-file-form ,(second pair))))
	  variable-pathspec-pairs)
    ,@forms))

(defun change-directory (pathspec)
  "Change current working directory. Keep the process'
current working directory and *default-pathname-defaults*
synchronized."
  (with-directory-form ((directory (merge-pathnames pathspec)))
    (if (path-exists-p directory)
	(progn
	  (setf (osicat:current-directory) directory)
	  (setf *default-pathname-defaults* directory))
	(error "Directory ~S does not exist." directory))))

(defun current-directory ()
  "Return the current working directory."
  *default-pathname-defaults*)

(defun change-directory-up ()
  "Same as cd .. in Unix. If current directory is root
root stays current directory."
  (change-directory (get-parent-directory (current-directory))))

(defmacro in-directory (directory &body forms)
  "Evaluate forms in directory and change back
to old working directory afterwards. The old working directory is
restored in any case."
  (with-gensyms (current-directory)
    `(let ((,current-directory (current-directory)))
      (unwind-protect
	   (progn
	     (change-directory ,directory)
	     ,@forms)
	(change-directory ,current-directory)))))

(defun delete-directory-tree (pathspec)
  "pathspec is interpreted as directory-form
and deleted -- along with all files and subdirectories."
  (with-directory-form ((directory pathspec))
    (let ((listing (directory-listing directory)))
      (dolist (entry listing)
	(if (directory-pathname-p entry)
	    (delete-directory-tree entry)
	    (delete-file entry)))
	(osicat:delete-directory directory))))

(defun create-directory (pathspec &key (require-fresh-directory nil))
  "pathspec is interpreted as a pathname in directory-form. Then all
directories in this path are created. If require-fresh-directory is T
an error is signalled if pathspec already existed."
  (with-directory-form ((directory pathspec))
    (let ((absolute-directory (merge-pathnames directory)))
      (multiple-value-bind (path created)
	  (ensure-directories-exist (merge-pathnames absolute-directory))
	(if (and require-fresh-directory (not created))
	    (error "Could not create fresh directory ~S, already existent."
		   absolute-directory)
	    path)))))

(defun find-all-files (pathspec)
  "pathspec is interpreted as a directory and a list
of all files with full pathname below this directory is
returned. That means all pathnames are in file form."
  (with-directory-form ((directory pathspec))
    (let* ((listing (directory-listing directory))
	   (files (remove-if #'directory-pathname-p listing))
	   (directories (remove-if-not #'directory-pathname-p
				       listing))
	   (file-lists (mapcar #'find-all-files directories)))
      (append files (apply #'append file-lists)))))

(defun move-directory-tree (source-pathspec destination-pathspec)
  "source-pathspec and destination-pathspec are interpreted as
pathnames in directory form. All files and directories below
source-pathspec are moved below destination-pathspec. The
last directory of source-pathspec still exists afterwards."
  (with-directory-form ((source (merge-pathnames source-pathspec))
			(destination (merge-pathnames
				      destination-pathspec)))
    (let* ((all-sources (find-all-files source))
	   (all-sources-relative
	    (mapcar #'(lambda (file)
			(parse-namestring (enough-namestring file source)))
		    all-sources))
	   (all-destinations
	    (mapcar #'(lambda (file)
			(merge-pathnames file destination))
		    all-sources-relative)))
      (mapcar #'(lambda (source-file destination-file)
		  (ensure-directories-exist destination-file)
		  (move-file source-file destination-file))
	      all-sources all-destinations)
      (mapcar #'delete-directory-tree
	      (directory-listing source)))))

(defun copy-stream (in out &optional (buffer-size 8192))
  "Copies all data from in to until in is empty.
Streams' elements must be compatible types."
  (unless (subtypep (stream-element-type in) (stream-element-type out))
    (error "Incompatible types of streams' elements."))
  (loop with buffer = (make-array buffer-size
				  :element-type (stream-element-type in))
	for length = (read-sequence buffer in)
	until (= length 0)
	do (write-sequence buffer out :end length)
	finally (return t)))

(defun copy-file (source destination &key overwrite)
  "Copies a file from source to destination. Overwrites
a possibly existing file if overwrite is T.
T is returned if file was cpoied, NIL otherwise.
source is interpreted as a pathspec in file-form, destination may
be in file- or directory-form. If it is in directory-form
the filename from source is taken.
All directories in destination have to exist."
  (when (wild-pathname-p source)
    (error "Cannot copy a wild pathname."))
  (when (wild-pathname-p destination)
    (error "Cannot copy to a wil location."))
  (let* ((from (pathname-to-file-form source))
	 (to (if (directory-pathname-p destination)
		 (merge-pathnames
		  (make-pathname
		   :name (pathname-name from)
	   :type (pathname-type from))
		  destination)
		 destination)))
    (with-open-file (in from
			:direction :input
			:if-does-not-exist :error
			:element-type '(unsigned-byte 8))
      (with-open-file (out to
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists (when overwrite :supersede)
			   :element-type '(unsigned-byte 8))
	(when out
	  (copy-stream in out))))))

(defun move-file (source destination &key overwrite)
  "Moves a file from source to destination. If
overwrite is T and destination exists it is overwritten.
move-file returns T if file was actually moved, NIL
otherwise.
Rules from copy-file for source and destination apply.
This move works also for cross file-system moves on
Unix. SBCL's RENAME-FILE e. g. fails in such cases."
  (when (copy-file source destination :overwrite overwrite)
    (delete-file source)))

(defmacro in-temporary-directory ((&key temporary-pathname
					(always-cleanup t)) &body forms)
  "Save creation of a temporary directory (e. g. under /tmp),
evaluate forms with temporary directory as working directory,
change back to old working directory and throw away
the temporary directory-tree.
If always-cleanup is set to NIL the temporary directory is only
removed, if no non-local exit occurs. That means, if a
RETURN-FROM or something similair is executed in the macro's body
the temporary directory is not removed. The default behaviour is
to alway remove the temporary directory and all its contents.
If provided, temporary-pathname is bound to the pathname of the
temporary directory."
  (with-gensyms (temporary-directory normal-cleanup)
    `(let ((,temporary-directory (create-temporary-directory))
	   (,normal-cleanup nil))
       (unwind-protect
	    (progn
	      (in-directory ,temporary-directory
		,(if temporary-pathname
		     `(let ((,temporary-pathname ,temporary-directory))
			,@forms)
		     `(progn
			,@forms)))
	      (delete-directory-tree ,temporary-directory)
	      (setf ,normal-cleanup t))
	 (when ,always-cleanup
	   (unless ,normal-cleanup
	     (delete-directory-tree ,temporary-directory)))))))

(defmacro with-temporary-file ((stream &optional filename) &body forms)
  "This is like the with-temporary-file macro from osicat but allows
for binding the name of the temporary file to filename."
  (with-gensyms (stream-internal filename-internal)
    `(let ,(remove () `(,stream ,(when filename filename)))
      (multiple-value-bind (,stream-internal ,filename-internal)
	  (create-temporary-file)
	(unwind-protect
	     (progn
	       ,@(remove ()
			`((setf ,stream ,stream-internal)
			  ,(when filename
				 `(setf ,filename ,filename-internal))))
	       ,@forms)
	  (when ,stream-internal
	    (close ,stream :abort t)))))))

(defun create-temporary-file ()
  "Open a temporary file for read-write. The stream and the filename is
returned as multiple-values."
  (let ((foreign-string (c-tempnam)))
    (if (uffi:null-pointer-p foreign-string)
	(error "Unable to create a temporary file.")
	(let* ((temporary-name
		(uffi:convert-from-foreign-string foreign-string))
	       (temporary-file
		(pathname-to-file-form temporary-name))
	       (stream (open temporary-file :if-exists nil
			     :direction :io)))
	  (uffi:free-foreign-object foreign-string)
	  (if stream
	      (values stream temporary-file)
	      (error "Tried to open create temporary file ~S but
someone else was quicker." temporary-file))))))

(defun create-temporary-directory ()
  "Create a temporay directory and return its pathname.
The location of the temporary directory is system dependent,
it may be /tmp/ or /var/tmp/ for example.
The function tries to create the directory in a safe way:
POSIX tempnam is called and a pathname is only returned
if the directory did not exist, when creating it."
  (let ((foreign-string (c-tempnam)))
    (if (uffi:null-pointer-p foreign-string)
	(error "Unable to create a temporary directory.")
	(let* ((temporary-name
		(uffi:convert-from-foreign-string foreign-string))
	       (temporary-directory
		(pathname-to-directory-form temporary-name)))
	  (uffi:free-foreign-object foreign-string)
	  ;; Create directory.
	  (multiple-value-bind (pathname created)
	      (ensure-directories-exist temporary-directory)
	    ;; If it was not created someone else did
	    ;; it -- abortion.
	    (if created
		pathname
		(error "Tried to create a temporary directory
but someone else was quicker.")))))))

(defmacro ensure-string-pathname (pathspec)
  "SETF pathspec to its namestring if it is
a pathname."
  `(when (typep ,pathspec 'pathname)
    (setf ,pathspec (namestring ,pathspec))))

(defmacro with-pathname (variable-pathspec-pairs &body forms)
  "Assigns each variable the pathname denoted by a pathspec (may
be a pathname, open or closed stream or string)."
  `(let ,(mapcar
	  (lambda (pair)
	    `(,(first pair) (pathname ,(second pair))))
	  variable-pathspec-pairs)
    ,@forms))

(defun component-defined-p (component)
  "Test if a given component of a pathname is defined, i. e.
non-nil or unspecific."
  (and component (not (eql component :unspecific))))

(defun directory-pathname-p (pathspec)
  "Test if a given pathname is in directory form (i. e. its
:name and :type components are not defined)."
  (and
   (not (component-defined-p (pathname-name pathspec)))
   (not (component-defined-p (pathname-type pathspec)))
   pathspec))

(defun pathname-to-directory-form (pathspec)
  "If pathspec is not in directory form its :name and
:type component are appended to its :directory component,
i. e. it is converted into directory-form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Cannot convert pathnames with wildcards into directory form."))
    (if (not (directory-pathname-p pathname))
      (make-pathname
       :directory (append
		   (or (pathname-directory pathname) '(:relative))
		   `(,(file-namestring pathname)))
       :name nil
       :type nil
       :defaults pathname)
      pathname)))

(defun pathname-to-file-form (pathspec)
  "If pathspec is in directory-form it is converted to
file-form, i. e. the last directory-component is
put into the name- and type-components."
  (with-pathname ((pathname pathspec))
    (when (wild-pathname-p pathname)
      (error "Cannot convert pathnames with wildcards into file-form."))
    (if (directory-pathname-p pathname)
	(let*
	    ((directories (pathname-directory pathname))
	     (file-name (pathname (first (last directories)))))
	  (make-pathname
	   :directory (butlast directories)
	   :name (pathname-name file-name)
	   :type (pathname-type file-name)))
	pathname)))

(defun root-p (pathspec)
  "Checks if pathspec is the file system's root.
This function is tested with CLISP, GCL, SBCL and CMUCL.
It is not sure if it works with other Common Lisp
implementations."
  (and
   (eql nil (pathname-name pathspec))
   (eql nil (pathname-type pathspec))
   (= 1 (length (pathname-directory pathspec)))))

(defun get-parent-directory (pathspec)
  "Returns the parent directory of pathspec. The parent of
the root directory is the root itself."
  (if (root-p pathspec)
      pathspec
      (if (directory-pathname-p pathspec)
	  (make-pathname :defaults pathspec
			 :directory (butlast (pathname-directory pathspec)))
	  (make-pathname :defaults pathspec
			 :name nil
			 :type nil))))

(defun directory-wildcard (directory)
  "Returns a pathname with wildcards in the file name
element suitable to produce a directory listing
with DIRECTORY."
  (with-directory-form ((pathname directory))
    (make-pathname
     :name :wild
     :type #-clisp :wild #+ clisp :nil
     :defaults pathname)))

(defun directory-listing (directory)
  "Returns the directory listing as a pathname list including both
directories and files."
  (with-directory-form ((pathname directory))
    (when (wild-pathname-p pathname)
      (error "Can not list content of a wildcard path."))
    (let ((directory-wildcard (directory-wildcard pathname)))
      #+(or sbcl cmu lispworks)
      (directory directory-wildcard)
      #+openmcl
      (directory wildcard :directories t)
      #+allegro
      (directory wildcard :directories-are-files t)
      #+clisp
      (nconc
       (directory wildcard)
       (directory (subdirectory-wildcard wildcard)))
      #-(or sbcl cmu lispworks openmcl allegro clisp)
      (error "DIRECTORY-LISTING only available for SBCL, CMUCL, LispWorks, OpenMCL, Allegro, CLISP"))))

#+clisp
(defun subdirectory-wildcard (wildcard)
  "Returns a pathname to produce a directory listings with
all subdirectories of wildcard with CLISP."
  (make-pathname
   :name nil
   :type nil
   :directory (append (pathname-directory wildcard) (list :wild))
   :defaults wildcard))

(defun path-exists-p (pathspec)
  "Tests if the given pathspec exists (be it a file or a directory)
and return a pathname in either directory- or file-form if so."
  (with-pathname ((pathname pathspec))
    #+(or sbcl lispworks openmcl)
    (probe-file pathname)
    #+(or allegro cmucl)
    (or (probe-file (pathname-to-directory-form pathname))
	(probe-file pathname))
    #+clisp
    (or (ignore-errors
	  (probe-file (pathname-to-file-form pathname)))
	(ignore-errors
	  (when (ext:probe-directory (pathname-to-directory-form pathname))
	    pathname)))))

(defun pathname-prefixes (pathspec)
  "Given a pathspec /dir1/dir2/dir3/file a list of
all directory-prefixes is generated:
/, /dir1/, /dir1/dir2/, /dir1/dir2/dir3/."
  (with-pathname ((pathname pathspec))
    (let ((directory-prefixes
	   ;; Generate all prefixes in reverse order. The longest
           ;; prefix is first and all directories appear in reverse
	   ;; order, too.
	   (maplist #'identity (reverse (pathname-directory pathname))))
	  pathname-prefixes)
      (dolist (prefix directory-prefixes)
	;; By using push, the shortest prefix is first in list and
	;; by reversing prefix, directories are put in correct order.
	(push (make-pathname :directory (reverse prefix)
			     :name nil :type nil
			     :defaults pathname) pathname-prefixes))
      pathname-prefixes)))

(defun pathname-prefix-p (prefix pathname)
  "Returns if prefix is a pefix of pathname. This done by
converting both pathnames to lists -- cf. pathname-to-list --
and checking for list-prefixes."
  (let ((pathname-list (pathname-to-list pathname))
	(prefix-list (pathname-to-list prefix)))
    (list-prefix-p prefix-list pathname-list)))
  
(defun pathname-to-list (pathname)
  "Converts a pathname to a list. The order of elements
is host, device, directory-components, name, type."
  (remove nil
	  (append (list (pathname-host pathname)
			(pathname-device pathname))
		  (pathname-directory pathname)
		  (list (pathname-name pathname)
			(pathname-type pathname)))))
    
(defun list-prefix-p (prefix list &key (test-function #'equal))
  "Check if prefix is a prefix of list. Equality of list
elements if tested by using test-function."
  (cond
    ((null prefix)
     t)
    ((> (length prefix) (length list))
     nil)
    ((funcall test-function (first prefix) (first list))
     (list-prefix-p (rest prefix) (rest list)
		    :test-function test-function))
    (t nil)))

;; Taken from Peter Seibel'a book.
(defun walk-directory (dirname fn &key dirs (test (constantly t)))
  (labels ((walk (name)
	     (cond 
	       ((directory-pathname-p name)
		(when (and dirs (funcall test name))
		  (funcall fn name))
		(dolist (x (directory-listing name))
		  (walk x)))
	       ((funcall test name) 
		(funcall fn name)))))
    (walk (pathname-to-directory-form dirname))))
		
(defun parent-dirs (&optional (path *default-pathname-defaults*))
  "Creates a list of all parent dirs from given dir. Default is 
*default-pathname-defaults*"
  (loop
     for x = path then (get-parent-directory x)
     collect x
     until (root-p x)))

(defun search-directory-in-directories (name path-list)
  "Searches for directory name in path-list.
returns a list of found directories."
  (let ((search-dir (make-pathname :directory `(:relative ,name))))
    (remove-if-not #'(lambda (p) (path-exists-p p))
		   (mapcar #'(lambda (p) (merge-pathnames search-dir p))
			   path-list))))

(defun append-pathnames (&rest pathspecs)
  "Appends all given pathspecs to a single pathspec. All but the
last argument are converted to directory-form, the last one is
used as directory-form if and only if it is in directory-form.
Thus /a/b /c/d e/f/g/ h/i is turned into /a/b/c/d/e/f/g/h/i."
  (when pathspecs
    (flet ((extract-directories (pathspec &optional process)
	     (let ((processed (if process
				  (funcall process pathspec)
				  pathspec)))
	       (remove-if-not
		#'stringp (pathname-directory processed)))))
      (let* ((directories
	      (append
	       (reduce #'append
		       (mapcar
			#'(lambda (pathspec)
			    (extract-directories pathspec
						 #'pathname-to-directory-form))
			       (butlast pathspecs)))
	       (extract-directories (car (last pathspecs)))))
	     (last (car (last pathspecs)))
	     (relative-absolute (first (pathname-directory (first pathspecs))))
	     (directory-component (append (list relative-absolute)
					  directories)))
	(if (directory-pathname-p last)
	    (make-pathname :directory directory-component)
	    (make-pathname :directory directory-component
			   :name (pathname-name last)
			   :type (pathname-type last)))))))
