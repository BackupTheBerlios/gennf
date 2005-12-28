;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "chatter")
;(require "find-bind")
;(provide "error")

(defvar *mcvs-error-treatment* :interactive
"This variable is used by the top level error handler set up in mcvs-execute to
decide on what to do with a restartable error condition.  If no restarts are
available, then this variable is ignored; the handler will print the error
message and terminate the program.  If the error is restartable, then this
variable is examined. A value of :interactive indicates that a menu of options
should be presented to a user, who can choose to terminate the program,
or invoke one of the available restarts. A value of :continue means
to emit a warning message and then invoke the a continue restart if
one is available. If restarts are available, but not ones that can
be automatically selected by the handler, then it will terminate the
program. A value of :terminate means to terminate on error, restartable
or not. A value of :decline means to return normally handling the error.")

(defvar *mcvs-errors-occured-p* nil)

(defvar *interactive-error-io* nil)

(defun mcvs-terminate (condition)
  (format *error-output* "mcvs: ~a~%" condition)
  (throw 'mcvs-terminate t))

(defun mcvs-error-handler (condition)
  (let ((*print-escape* nil))
    (setf *mcvs-errors-occured-p* t)
    (find-bind (:key #'restart-name)
	       (others (continue 'continue)
		       (info 'info)
		       (retry 'retry))
	       (compute-restarts)
      (ecase *mcvs-error-treatment*
	((:interactive)
	   (unless *interactive-error-io*
	     (return-from mcvs-error-handler nil))
	   (when (null (compute-restarts))
	     (mcvs-terminate condition))
	   (let* (command-list
		  (menu (with-output-to-string (stream)
			  (format stream "~%The following error has occured:~%~%")
			  (format stream "    ~a~%~%" condition)
			  (format stream "You have these alternatives:~%~%")
			  (format stream "    ?) Re-print this menu.~%" continue)
			  (when info
			    (format stream "    I) (Info) ~a~%" info)
			    (push (list "I" #'(lambda () 
						(invoke-restart info)))
				  command-list))
			  (when continue
			    (format stream "    C) (Continue) ~a~%" continue)
			    (format stream "    A) Auto-continue all continuable errors.~%")
			    (push (list "C" #'(lambda () 
						(invoke-restart continue)))
				  command-list)
			    (push (list "A" #'(lambda () 
						(setf *mcvs-error-treatment*
						      :continue)
						(invoke-restart continue)))
				  command-list))
			  (when retry
			    (format stream "    R) (Retry) ~a~%" retry)
			    (push (list "R" #'(lambda () 
						(invoke-restart retry)))
				  command-list))
			  (format stream "    T) Recover, clean-up and terminate.~%")
			  (push (list "T" #'(lambda ()
					      (throw 'mcvs-terminate t)))
				command-list)
			  (when others
			    (format stream "~%These special alternatives are also available:~%~%")
			    (let ((counter 0))
			      (dolist (restart others)
				(format stream "    ~a) ~a~%" (incf counter) restart)
				(push (list (format nil "~a" counter)
					    (let ((restart restart))
					      #'(lambda () 
						  (invoke-restart restart))))
				      command-list))))
			  (terpri stream))))
	     (write-string menu *interactive-error-io*)
	     (force-output *interactive-error-io*)
	     (loop 
	       (write-string ">" *interactive-error-io*)
	       (let* ((line (read-line *interactive-error-io*))
		      (command (find line command-list 
				     :key #'first 
				     :test #'string-equal)))
		 (cond
		   ((string= line "?")
		     (write-string menu *interactive-error-io*))
		   (command
		     (funcall (second command)))
		  (t (format *interactive-error-io* "What?~%")))))))
	((:continue)
	   (when continue
	     (chatter-terse "Auto-continuing error:~%")
	     (chatter-terse "    ~a~%" condition)
	     (invoke-restart continue))
	   (mcvs-terminate condition))
	((:terminate)
	   (mcvs-terminate condition))
	((:decline)
	   (return-from mcvs-error-handler nil))))))
