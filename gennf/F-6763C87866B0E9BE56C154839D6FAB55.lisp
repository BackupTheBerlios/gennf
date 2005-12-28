;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "system")
;(require "dirwalk")
;(require "mapping")
;(require "find-bind")

(defun mcvs-purge (global-options)
  (in-sandbox-root-dir
    (let* ((filemap (mapping-read *mcvs-map* :sanity-check t))
	   (to-be-removed (mapping-removed-files filemap)))
      (when to-be-removed
	(chdir *mcvs-dir*)
	 (chatter-debug "Invoking CVS.~%")
	 (unless (execute-program-xargs `("cvs" ,@(format-opt global-options)
					  "rm" "-f")
					(mapcar #'basename to-be-removed))
	   (error "CVS rm failed.")))))
  (values))

(defun mcvs-purge-wrapper (global-options command-options args)
  (declare (ignore command-options))
  (when args
    (error "no arguments permitted."))
  (mcvs-purge global-options))
