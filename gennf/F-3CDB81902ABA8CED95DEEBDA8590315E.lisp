;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(provide "chatter")

(define-constant *mcvs-debug* 3)
(define-constant *mcvs-info* 2)
(define-constant *mcvs-terse* 1)
(define-constant *mcvs-silent* 0)

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
