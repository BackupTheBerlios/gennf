;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku
;;; und ich

(in-package :gennf)

;(require "generic")
;(provide "watch")

(defun mcvs-watch (mcvs-opts watch-opts files)
  (when (> (length watch-opts) 1)
    (error "only one option can be specified"))
  (find-bind (:test #'string= :key #'first)
	     ((on "on") (off "off")
	      (add "add") (remove "remove"))
	     watch-opts
    (let ((watch-args (cond
			(on '("on"))
			(off '("off"))
			(add `("add" "-a" ,(second add)))
			(remove `("remove" "-a" ,(second remove)))
			(t (error "no watch option specified")))))
      (mcvs-generic "watch" mcvs-opts nil watch-args files))))

(defun mcvs-watch-wrapper (mcvs-opts command-opts command-args)
  (mcvs-watch mcvs-opts command-opts command-args))
