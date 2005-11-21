;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "chatter")
(provide "clisp-unix")

;;; Null pointer handling

#.(when (< (first (system::version)) 20020129)
    (push :clisp-old *features*)
    (values))

#+clisp-old
  (defmacro pointer-null (p) `(unix-funcs:null-pointer-p ,p))

#-clisp-old
  (defmacro pointer-null (p) `(ffi:foreign-address-null ,p))

(defmacro null-to-nil (p) 
  (let ((pointer (gensym)))
    `(let ((,pointer ,p)) (if (pointer-null ,pointer) nil ,pointer))))

(defmacro when-not-null (p &body forms)
  `(if (not (pointer-null ,p)) ,@forms))

;;; Base condition

(define-condition system-error (error) ((message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "System error: ~A" 
	       (slot-value condition 'message)))))

;;; Macro to catch ENOENT errors and turn them into nil 
;;; return value.

(defmacro no-existence-error (&body forms)
  (let ((block-sym (gensym "BLOCK-")))
   `(block ,block-sym
     (handler-bind
       ((system-error #'(lambda (con)
			  (declare (ignore con))
			  (if (= unix-funcs:errno unix-funcs:enoent)
			    (return-from ,block-sym nil)))))
	 ,@forms))))

;;; Directory access

(define-condition open-dir-error (system-error) ((dir :initarg :dir)))

(defmethod initialize-instance :after ((c open-dir-error) &rest args)
  (declare (ignore args))
  (with-slots (dir message) c
    (setf message (format nil "Unable to open ~A: ~A." 
                          dir (unix-funcs:strerror unix-funcs:errno)))))

(define-condition open-error (system-error) ((path :initarg :path)))

(defmethod initialize-instance :after ((c open-error) &rest args)
  (declare (ignore args))
  (with-slots (path message) c
    (setf message (format nil "Unable to open ~A: ~A." 
                          path (unix-funcs:strerror unix-funcs:errno)))))
   
   
(defun opendir (dir) 
  (cond
    ((null-to-nil (unix-funcs:opendir dir)))
    (t (error (make-condition 'open-dir-error :dir dir)))))

(declaim (inline closedir))
(defun closedir (dir-stream) 
  (when-not-null dir-stream (unix-funcs:closedir dir-stream)))


(defun readdir (dir-stream) 
  (let ((dir-entry (unix-funcs:readdir dir-stream)))
    (if dir-entry
      (with-slots ((name unix-funcs:name) (ino unix-funcs:ino)) dir-entry
        (values name ino))
      nil)))

(defmacro with-open-dir ((var dir) &body forms)
 `(let ((,var (opendir ,dir)))
    (unwind-protect
      (progn ,@forms)
      (closedir ,var))))

(define-condition chdir-error (system-error) ((dir :initarg :dir)))

(defmethod initialize-instance :after ((c chdir-error) &rest args)
  (declare (ignore args))
  (with-slots (dir message) c
    (setf message (format nil "Unable to change to directory ~A: ~A." 
                          dir (unix-funcs:strerror unix-funcs:errno)))))

(defun chdir (dir)
  (if (= -1 (unix-funcs:chdir dir))
    (error (make-condition 'chdir-error :dir dir)))
    (values))

(defun fchdir (descr)
  (if (= -1 (unix-funcs:fchdir descr))
    (error (make-condition 'chdir-error 
                           :dir (format nil "[file descriptor ~a]" descr))))
    (values))

(define-condition getcwd-error (system-error) ())

(defmethod initialize-instance :after ((c getcwd-error) &rest args)
  (declare (ignore args))
  (with-slots (message) c
    (setf message (format nil "Unable to determine current directory: ~A." 
                          (unix-funcs:strerror unix-funcs:errno)))))

(declaim (inline getcwd))
(defun getcwd ()
  (or (unix-funcs:getcwd)
      (error (make-condition 'getcwd-error))))

(defmacro current-dir-restore (&body forms)
  (let ((saved-dir (gensym "SAVED-DIR-")))
    `(let ((,saved-dir (unix-funcs:open "." unix-funcs:o-rdonly 0)))
       (when (= ,saved-dir -1)
	 (error (make-condition 'open-error :path ".")))
       (unwind-protect 
	 (macrolet ((in-original-dir (&body inner-forms)
		      (let ((in-saved-dir (gensym "INNER-SAVED-DIR-")))
		        `(let ((,in-saved-dir 
				 (unix-funcs:open "." 
						  unix-funcs:o-rdonly 
						  0)))
			   (when (= ,in-saved-dir -1)
			     (error (make-condition 'open-error :path ".")))
			   (unwind-protect
			     (progn (fchdir ,',saved-dir)
				    (progn ,@inner-forms))
			     (fchdir ,in-saved-dir)
			     (unix-funcs:close ,in-saved-dir))))))
	   ,@forms)
	 (fchdir ,saved-dir)
	 (unix-funcs:close ,saved-dir)))))
  
;;; File information

(define-condition file-info-error (system-error) ((file :initarg :file)))

(defmethod initialize-instance :after ((c file-info-error) &rest args)
  (declare (ignore args))
  (with-slots (file message) c
    (setf message (format nil "Unable to get status of ~A: ~A." 
                          file (unix-funcs:strerror unix-funcs:errno)))))

(defclass file-info ()
  ((file-name :initarg :file-name :accessor file-name)
   (mode-flags :initarg :mode-flags :accessor mode-flags)
   (mod-time :initarg :mod-time :accessor mod-time)
   (inode :initarg :inode :accessor inode)
   (num-links :initarg :num-links :accessor num-links)))

(defgeneric same-file-p (file1 file2))
(defgeneric older-p (file1 file2))
(defgeneric regular-p (file))
(defgeneric directory-p (file))
(defgeneric symlink-p (file))
(defgeneric is-root-p (file))
(defgeneric get-parent (file))
(defgeneric executable-p (file))
(defgeneric make-executable (file))
(defgeneric make-non-executable (file))

(defmethod same-file-p ((f1 file-info) (f2 file-info))
  (= (inode f1) (inode f2)))

(defmethod same-file-p ((f1 string) (f2 string))
  (= (stat f1) (stat f2)))

(defmethod older-p ((f1 file-info) (f2 file-info))
  (< (mod-time f1) (mod-time f2)))

(defmethod older-p ((f1 string) (f2 string))
  (older-p (stat f1) (stat f2)))

(defmethod regular-p ((file file-info))
  (unix-funcs:s-isreg (mode-flags file)))

(defmethod regular-p ((filename string))
  (regular-p (stat filename)))

(defmethod regular-p ((x null))
  nil)

(defmethod directory-p ((file file-info))
  (unix-funcs:s-isdir (mode-flags file)))

(defmethod directory-p ((filename string))
  (directory-p (stat filename)))

(defmethod directory-p ((x null))
  nil)

(defmethod symlink-p ((file file-info))
  (unix-funcs:s-islnk (mode-flags file)))

(defmethod symlink-p ((filename string))
  (symlink-p (stat filename)))

(defmethod symlink-p ((x null))
  nil)

(defmethod is-root-p ((file file-info))
  (and (directory-p file)
       (same-file-p file (stat (format nil "~a/.." (file-name file))))))

(defmethod is-root-p ((filename string))
  (is-root-p (stat filename)))

(defmethod get-parent ((file file-info))
  (stat (format nil "~a/.." (file-name file))))

(defmethod get-parent ((filename string))
  (stat (format nil "~a/.." filename)))

(defmethod executable-p ((file file-info))
  (with-slots ((mode mode-flags)) file
    (and (not (zerop (logand mode unix-funcs:s-ixusr)))
	 (not (zerop (logand mode unix-funcs:s-ixgrp))))))

(defmethod executable-p ((filename string))
  (executable-p (stat filename)))

(defmethod make-executable ((file file-info))
  (with-slots ((mode mode-flags) file-name) file
    (let ((saved-mode mode))
      (unless (zerop (logand mode unix-funcs:s-irusr))
	(setf mode (logior mode unix-funcs:s-ixusr)))
      (unless (zerop (logand mode unix-funcs:s-irgrp))
	(setf mode (logior mode unix-funcs:s-ixgrp)))
      (unless (zerop (logand mode unix-funcs:s-iroth))
	(setf mode (logior mode unix-funcs:s-ixoth)))
      (unless (= mode saved-mode)
	(unix-funcs:chmod file-name mode)))))

(defmethod make-executable ((filename string))
  (make-executable (stat filename)))

(defmethod make-non-executable ((file file-info))
  (with-slots ((mode mode-flags) file-name) file
    (let ((saved-mode mode))
      (setf mode (logand mode 
			 (lognot (logior unix-funcs:s-ixusr
					 unix-funcs:s-ixgrp
					 unix-funcs:s-ixoth))))
      (unless (= mode saved-mode)
	(unix-funcs:chmod file-name mode)))))

(defmethod make-non-executable ((filename string))
  (make-non-executable (stat filename)))

(defun stat (name &key through-link)
  (if (typep name 'file-info)
    name
    (multiple-value-bind (result stat-info) 
			 (if through-link 
			    (unix-funcs:stat name)
			    (unix-funcs:lstat name))
      (when (= result -1)
	(error (make-condition 'file-info-error :file name)))
      (with-slots ((mode unix-funcs:mode) 
		   (mtime unix-funcs:mtime)
		   (inode unix-funcs:ino)
		   (nlink unix-funcs:nlink)) stat-info
	(make-instance 'file-info :file-name name
				  :mode-flags mode
				  :mod-time mtime
				  :inode inode
				  :num-links nlink)))))

(defun exists (name &key through-link)
  (no-existence-error (stat name :through-link through-link)))

;;; Symbolic and hard links

(define-condition link-error (system-error) 
  ((from-path :initarg :from-path)
   (to-path :initarg :to-path)
   (kind :initarg :kind)))

(defmethod initialize-instance :after ((c link-error) &rest args)
  (declare (ignore args))
  (with-slots (message kind from-path to-path) c
    (setf message (format nil "Unable to make ~A link called ~A referencing ~A."
                          kind to-path from-path
			  (unix-funcs:strerror unix-funcs:errno)))))

(defun link (from to)
  (if (zerop (unix-funcs:link from to))
    (values)
    (error (make-condition 'link-error :from-path from 
			   :to-path to :kind "hard"))))

(defun symlink (from to)
  (if (zerop (unix-funcs:symlink from to))
    (values)
    (error (make-condition 'link-error :from-path from 
			   :to-path to :kind "symbolic"))))

(define-condition readlink-error (system-error) 
  ((path :initarg :path)))

(defmethod initialize-instance :after ((c readlink-error) &rest args)
  (declare (ignore args))
  (with-slots (message path) c
    (setf message (format nil "Unable to read symbolic link ~A: ~A." 
                          path (unix-funcs:strerror unix-funcs:errno)))))

(defun readlink (path)
  (let ((data (unix-funcs:readlink path)))
    (if data
      data
      (error (make-condition 'readlink-error :path path)))))

;;; Directory removal

(define-condition rm-error (system-error) 
  ((path :initarg :path)))

(defmethod initialize-instance :after ((c rm-error) &rest args)
  (declare (ignore args))
  (with-slots (message path) c
    (setf message (format nil "Unable to remove ~A: ~A." 
                          path (unix-funcs:strerror unix-funcs:errno)))))

(defun rmdir (dir)
  (if (zerop (unix-funcs:rmdir dir))
    (values)
    (error (make-condition 'rm-error :path dir))))

(defun unlink (file)
  (if (zerop (unix-funcs:unlink file))
    (values)
    (error (make-condition 'rm-error :path file))))

;;; Coprocesses

(defun shell-interpreter (command)
  (case (shell command)
    ((0) T)
    (otherwise nil)))

(defun execute-program (arglist)
  (chatter-debug "invoking ~s in directory ~s~%" arglist (getcwd))
  (case (unix-funcs:run-program (first arglist) :arguments (rest arglist))
    ((0) (chatter-debug "successful termination~%") T)
    (otherwise (chatter-debug "unsuccessful or abnormal termination~%") nil)))

(defmacro with-input-from-program ((stream-var arg-list) &body forms)
  (let ((arg-list-sym (gensym "ARG-LIST-")))
    `(let ((,arg-list-sym ,arg-list))
       (chatter-debug "piping from ~s in directory ~s~%" ,arg-list-sym (getcwd))
       (unix-funcs:default-sigchld)
       (let* ((,stream-var (make-pipe-input-stream 
			     (arglist-to-command-string ,arg-list-sym))))
	 (declare (dynamic-extent ,stream-var))
	 (when ,stream-var
	   (unwind-protect (progn ,@forms) (close ,stream-var)))))))

(defmacro with-output-to-program ((stream-var arg-list) &body forms)
 `(progn
    (unix-funcs:default-sigchld)
    (let* ((,stream-var (make-pipe-output-stream 
			(arglist-to-command-string ,arg-list))))
      (declare (dynamic-extent ,stream-var))
      (when ,stream-var
	(unwind-protect (progn ,@forms) (close ,stream-var))))))

;;; GUID generation

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

;;; Environment strings
(defun env-lookup (name &optional substitute-if-not-found)
  (let ((value (getenv name)))
    (if value value substitute-if-not-found)))
