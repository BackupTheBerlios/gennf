;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "restart")
(provide "dirwalk")

;; TODO: this sucks, it should put out canonicalized path names
(defun dirwalk-fi (dir-fi func &rest keys &key norecurse postorder)
  (let ((dir-path (file-name dir-fi)))
    (setf dir-path (cond
		     ((string-equal dir-path "") 
		        #.(format nil "~a~a" *this-dir* *path-sep*))
		     ((eql (char dir-path (1- (length dir-path))) *path-sep*) 
		        dir-path)
		     (t (format nil "~a~a" dir-path *path-sep*))))
    (catch 'dirwalk-skip
      (when (not postorder)
        (funcall func dir-fi))
      (with-open-dir (d dir-path)
	(loop
	  (let ((name (readdir d)) entry-path fi)
	    (can-restart-here ("Continue processing directory ~a." dir-path)
	      (cond
		((null name) (return))
		((string-equal name *this-dir*) nil)
		((string-equal name *up-dir*) nil)
		((and (setf entry-path (format nil "~a~a" dir-path name))
		      (setf fi (stat entry-path))
		      nil))
		((and (not norecurse) (directory-p fi))
		 (apply #'dirwalk-fi fi func keys))
		(t (funcall func fi)))))))
      (when postorder
	(funcall func dir-fi)))))

(defun dirwalk (dir-path func &rest keys &key norecurse postorder)
  (declare (ignore norecurse postorder))
  (let ((fi (stat dir-path)))
     (if (directory-p fi)
       (apply #'dirwalk-fi fi func keys)
       (catch 'dirwalk-skip (funcall func fi)))))

(defun map-path (dir-path func)
  (dirwalk dir-path #'(lambda (x) (funcall func (file-name x)))))

(defmacro for-each-path ((var dirpath) &body forms)
  (let ((file-info (gensym "FILE-INFO-")))
   `(dirwalk ,dirpath #'(lambda (,file-info) 
			  (flet ((skip () (throw 'dirwalk-skip nil)))
			    (let ((,var (file-name ,file-info))) 
			      ,@forms))))))

(defmacro for-each-file-info ((var dirpath &rest keys 
			       &key norecurse postorder) &body forms)
  (declare (ignore norecurse postorder))
   `(dirwalk ,dirpath #'(lambda (,var) 
			  (flet ((skip () (throw 'dirwalk-skip nil)))
			    ,@forms)) ,@keys))

(defun delete-recursive (dir-or-file)
  (for-each-file-info (fi dir-or-file :postorder t)
    (if (directory-p fi)
      (rmdir (file-name fi))
      (unlink (file-name fi)))))

(defun ensure-directories-gone (dir-or-file-to-erase)
"Intended as the inverse of Common Lisp's ensure-directories-exist, this
function erases the specified file. Then it tries to erase the parent
directory. If that succeeds, then it tries to erase the grandparent parent
directory and so on, until it encounters a directory that cannot be removed."
  (if (directory-p dir-or-file-to-erase)
    (rmdir dir-or-file-to-erase)
    (unlink dir-or-file-to-erase))

  (multiple-value-bind (base dir) (basename (canonicalize-path 
					     dir-or-file-to-erase))
    (declare (ignore base))
    (handler-case
      (loop
	(rmdir dir)
	(setf dir (canonicalize-path (path-cat dir *up-dir*))))
      (error (x) (declare (ignore x)) (values)))))
