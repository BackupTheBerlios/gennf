;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(provide "chatter")

(defconstant *mcvs-debug* 3)
(defconstant *mcvs-info* 2)
(defconstant *mcvs-terse* 1)
(defconstant *mcvs-silent* 0)

(defvar *mcvs-chatter-level* *mcvs-info*)
(declaim (special *mcvs-chatter-level*))

(defun chatter (level &rest args)
  (when (>= *mcvs-chatter-level* level)
    (write-string "* " *error-output*)
    (apply #'format *error-output* args)))

(defmacro chatter-debug (&rest args)
  `(chatter *mcvs-debug* ,@args))

(defmacro chatter-info (&rest args)
  `(chatter *mcvs-info* ,@args))

(defmacro chatter-terse (&rest args)
  `(chatter *mcvs-terse* ,@args))
