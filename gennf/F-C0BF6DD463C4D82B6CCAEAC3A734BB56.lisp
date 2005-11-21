;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "mapping")
(require "options")
(provide "filt")

(defun make-filt-hash (mapping)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (entry mapping h)
      (multiple-value-bind (suffix nosuffix)
			   (suffix (mapping-entry-id entry))
	(declare (ignore suffix))
	(setf (gethash nosuffix h) entry)))))

(defun filt-select-map (filt-options &key remote-module)
  (find-bind (:test #'string= :key #'first :take #'second)
	     ((revision "r") (date "D") (extra-r "r") (extra-d "D")) 
	     filt-options
    (cond
      ((or extra-r extra-d)
	 (error "only one date or revision may be specified."))
      ((or revision date remote-module)
	 (unless remote-module
           (chdir *mcvs-dir*))
	 (with-input-from-program (stream `("cvs" "-Q" 
					    ,(if remote-module "co" "up") "-p"
					    ,@(format-opt filt-options)
					    ,(if remote-module 
					       (format nil "~a/~a"
						       remote-module
						       *mcvs-map-name*)
					       *mcvs-map-name*)))
	   (handler-case
	     (mapping-read stream)
	     (error ()
	       (error "unable to retrieve specified revision of map file.")))))
      (t (mapping-read *mcvs-map-local*)))))

(defun mcvs-filt-loop (filehash)
  (loop
    (let ((line (read-line *standard-input* nil)))	
      (when (null line) 
	(return (values)))
      (loop
	(let ((f-start (search "F-" line :test #'char=))
	      (embedded-in-path (search "/F-" line :test #'char=)))
	  (flet ((is-hex-digit (x) 
		   (or (digit-char-p x)
		       (find x "ABCDEF"))))
	    (cond
	      ((and embedded-in-path (or (and f-start 
					      (< embedded-in-path f-start))
					 (not f-start)))
		(write-string (substring line 0 (+ embedded-in-path 7)))
		(setf line (substring line (+ embedded-in-path 7))))
	      (f-start
		(write-string (substring line 0 f-start))
		(setf line (substring line (+ f-start 2)))
		(when (< (length line) 32)
		  (write-string "F-")
		  (write-line line)
		  (return))
		(cond 
		  ((notevery #'is-hex-digit (substring line 0 32))
		     (write-string "F-")
		     (setf line (substring line 2)))
		  (t (let* ((f-digits (substring line 0 32))
			    (entry (gethash (format nil "F-~a" f-digits)
					    filehash))
			    (suffix (and entry 
					 (suffix (mapping-entry-id entry)))))
		       (setf line (substring line 32))
		       (cond
			 ((null entry)
			    (write-string "F-")
			    (write-string f-digits))
			 ((and suffix 
			       (or (< (length line) (1+ (length suffix)))
				   (not (path-equal (substring line 1 
							       (1+ (length suffix)))
						    suffix))))
			    (write-string "F-")
			    (write-string f-digits))
			 (t (write-string (mapping-entry-path entry))
			    (when suffix
			      (setf line 
				    (substring line 
					       (1+ (length suffix)))))))))))
	      (t (write-line line)
		 (return)))))))))

(defun mcvs-filt (filt-options)
  (in-sandbox-root-dir
    (mcvs-filt-loop (make-filt-hash (filt-select-map filt-options)))))

(defun mcvs-remote-filt (filt-options module)
  (mcvs-filt-loop (make-filt-hash (filt-select-map filt-options 
						   :remote-module module))))


(defun mcvs-filt-wrapper (cvs-options cvs-command-options mcvs-args)
  (declare (ignore cvs-options))
  (when mcvs-args
    (error "no arguments permitted."))
  (mcvs-filt cvs-command-options))

(defun mcvs-remote-filt-wrapper (cvs-options cvs-command-options mcvs-args)
  (declare (ignore cvs-options))
  (unless (= (length mcvs-args) 1)
    (error "module name required."))
  (mcvs-remote-filt cvs-command-options (first mcvs-args)))
