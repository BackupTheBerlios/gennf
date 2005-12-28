;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "posix")
(provide "cmucl-unix")

;;; Base condition

(define-condition system-error (error) ((message :initarg :message :reader message))
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
			  (if (= unix:unix-errno unix:ENOENT)
			    (return-from ,block-sym nil)))))
	 ,@forms))))

;;; Directory access

(define-condition open-dir-error (system-error) ((dir :initarg :dir)))

(defmethod initialize-instance :after ((c open-dir-error) &rest args)
  (declare (ignore args))
  (with-slots (dir message) c
    (setf message (format nil "Unable to open ~A: ~A." 
                          dir (aref unix::*unix-errors* (unix:unix-errno))))))

(define-condition open-error (system-error) ((path :initarg :path)))

(defmethod initialize-instance :after ((c open-error) &rest args)
  (declare (ignore args))
  (with-slots (path message) c
    (setf message (format nil "Unable to open ~A: ~A." 
                          path (aref unix::*unix-errors* (unix:unix-errno))))))
   
   
(defun opendir (dir) 
  (cond
    ((unix:open-dir dir))
    (t (error (make-condition 'open-dir-error :dir dir)))))

(declaim (inline closedir))
(defun closedir (dir-stream) 
  (when dir-stream (unix:close-dir dir-stream)))

(declaim (inline readdir))
(defun readdir (dir-stream) 
  (unix:read-dir dir-stream))


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
                          dir (aref unix::*unix-errors* (unix:unix-errno))))))

(defun chdir (dir)
  (if (not (unix:unix-chdir dir))
    (error (make-condition 'chdir-error :dir dir)))
    (values))

(defmacro current-dir-restore (&body forms)
  (let ((saved-dir (gensym "SAVED-DIR-"))
        (getdir-ok (gensym "GETDIR-OK-")))
    `(multiple-value-bind (,getdir-ok ,saved-dir)
			  (unix:unix-current-directory)
       (when (not ,getdir-ok)
	 (error "could not determine current working directory"))
       (unwind-protect (progn ,@forms)
                       (chdir ,saved-dir)))))
  
;;; File information

(define-condition file-info-error (system-error) ((file :initarg :file)))

(defmethod initialize-instance :after ((c file-info-error) &rest args)
  (declare (ignore args))
  (with-slots (file message) c
    (setf message (format nil "Unable to get status of ~A: ~A." 
                          file (aref unix::*unix-errors* (unix:unix-errno))))))

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

(defmethod same-file-p ((f1 file-info) (f2 file-info))
  (= (inode f1) (inode f2)))

(defmethod same-file-p ((f1 string) (f2 string))
  (= (stat f1) (stat f2)))

(defmethod older-p ((f1 file-info) (f2 file-info))
  (< (mod-time f1) (mod-time f2)))

(defmethod older-p ((f1 string) (f2 string))
  (older-p (stat f1) (stat f2)))

(defmethod regular-p ((file file-info))
  (not (zerop (logand unix:s-ifreg (mode-flags file)))))

(defmethod regular-p ((filename string))
  (regular-p (stat filename)))

(defmethod regular-p ((x null))
  nil)

(defmethod directory-p ((file file-info))
  (not (zerop (logand unix:s-ifdir (mode-flags file)))))

(defmethod directory-p ((filename string))
  (directory-p (stat filename)))

(defmethod directory-p ((x null))
  nil)

(defmethod symlink-p ((file file-info))
  (not (zerop (logand unix:s-iflnk (mode-flags file)))))

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

(defun stat (name &key through-link)
  (if (typep name 'file-info)
    name
    (multiple-value-bind (okay dev inode mode nlink owner group 
			  devnum size atime mtime ctime)
			 (if through-link 
			    (unix:unix-stat name)
			    (unix:unix-lstat name))
      (declare (ignore dev owner 
		       group devnum size atime ctime))
      (when (not okay)
	(error (make-condition 'file-info-error :file name)))
	  (make-instance 'file-info :file-name name
				    :mode-flags mode
				    :mod-time mtime
				    :inode inode
				    :num-links nlink))))

(defun exists (name &key through-link)
  (no-existence-error (stat name :through-link through-link)))

;;; Symbolic and hard links

(define-condition link-error (system-error) 
  ((from-path :initarg :from-path)
   (to-path :initarg :to-path)))

(defmethod initialize-instance :after ((c link-error) &rest args)
  (declare (ignore args))
  (with-slots (message from-path to-path) c
    (setf message (format nil "Unable to link ~A to ~A: ~A." 
                          from-path to-path
			  (aref unix::*unix-errors* (unix:unix-errno))))))

(defun link (from to)
  (if (unix:unix-link from to)
    (values)
    (error (make-condition 'link-error :from-path from :to-path to))))

;;; Directory removal

(define-condition rm-error (system-error) 
  ((path :initarg :path)))

(defmethod initialize-instance :after ((c rm-error) &rest args)
  (declare (ignore args))
  (with-slots (message path) c
    (setf message (format nil "Unable to remove ~A: ~A." 
                          path (aref unix::*unix-errors* (unix:unix-errno))))))

(defun rmdir (dir)
  (if (unix:unix-rmdir dir)
    (values)
    (error (make-condition 'rm-error :path dir))))

(defun unlink (file)
  (if (unix:unix-unlink file)
    (values)
    (error (make-condition 'rm-error :path file))))

;;; Coprocesses

(define-constant *shell-executable* "/bin/sh")

(defun shell-interpreter (command)
  (setf command (coerce command 'simple-string))
  (let ((pid (unix:unix-fork)))
    (cond
      ((< pid 0) ;; error
         (error "fork failed"))
      ((zerop pid) ;; child
         (unix:unix-execve *shell-executable* `(,*shell-executable*
						 "-c" ,command))
	 (unix:unix-exit 1))
      (t ; parent
	 (multiple-value-bind (pid-out event status) 
			      (extensions::wait3)
	   (and (= pid pid-out)
		(eq event :exited)
		(eq status 0)))))))

(defmacro with-input-from-program ((stream-var arg-list) &body forms)
 `(let* ((,stream-var (make-pipe-input-stream 
			(arglist-to-command-string ,arg-list))))
    (declare (dynamic-extent ,stream-var))
    (when ,stream-var
      (unwind-protect (progn ,@forms) (close ,stream-var)))))

(defmacro with-output-to-program ((stream-var arg-list) &body forms)
 `(let* ((,stream-var (make-pipe-output-stream 
			(arglist-to-command-string ,arg-list))))
    (declare (dynamic-extent ,stream-var))
    (when ,stream-var
      (unwind-protect (progn ,@forms) (close ,stream-var)))))

;;; GUID generation

(defun guid-gen ()
  (with-open-file (f "/dev/urandom" 
                   :direction :input 
		   :element-type '(unsigned-byte 128))
    (read-byte f)))
