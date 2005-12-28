;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "dirwalk")
;(require "chatter")
;(require "mapping")
;(require "options")
;(require "generic")
;(provide "update")

(defun mcvs-update (&optional cvs-options cvs-update-options files)
  (let ((need-sync (not (find "p" cvs-update-options 
			      :key #'first :test #'string=))))
    (if (or files *metaonly-option* *nometa-option*)
      (mcvs-generic "update" cvs-options cvs-update-options nil 
		    files :need-sync-after need-sync 
		    :need-update-after t)
      (in-sandbox-root-dir
	(if need-sync
	  (progn
	    ;; Push changes in tree to CVS sandbox, so they can be merged
	    ;; with stuff coming from repository.
	    (chatter-debug "Synchronizing.~%")
	    (mapping-synchronize :direction :left)

	    (current-dir-restore 
	      (chdir *mcvs-dir*) 
	      (super-restart-case
		(progn
		  (chatter-debug "Invoking CVS.~%")
		  (unless (execute-program `("cvs" ,@(format-opt cvs-options) 
					     "up" ,@(format-opt 
						      cvs-update-options)))
		    (error "CVS update failed.")))
		(continue () 
		  :report "Update file structure and re-synchronize." 
		  (unwind))
		(retry () 
		  :report "Try invoking CVS again." 
		  (retry))))

	    (chatter-debug "Updating file structure.~%")
	    (mapping-update)
	    (chatter-debug "Synchronizing again.~%")
	    (mapping-synchronize :direction :right))
	  (current-dir-restore 
	    (chdir *mcvs-dir*) 
	    (chatter-debug "Invoking CVS.~%")
	    (unless (execute-program `("cvs" ,@(format-opt cvs-options) 
				       "up" ,@(format-opt cvs-update-options))))
	      (error "CVS update failed."))))))
  (values))

(defun mcvs-update-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-update cvs-options cvs-command-options mcvs-args))
