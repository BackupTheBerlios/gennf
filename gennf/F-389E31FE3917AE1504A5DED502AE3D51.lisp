;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(provide "print")

(defun print-assoc-list (alist &optional (stream *standard-output*))
"Print an association list in this nice form
 ((a b ...)
  (c d ...)
  ...
  (e f ...))"
 (cond
   ((null alist)
      (write nil :stream stream))
   ((not (consp alist))
      (error "PRINT-ASSOC-LIST: ~a is not a list." alist))
   (t (format stream "(~s" (first alist))
      (loop
	(setf alist (rest alist))
	(when (null alist)
	  (write-string ")" stream)
	  (return))
	(format stream "~& ~s" (first alist))))))
