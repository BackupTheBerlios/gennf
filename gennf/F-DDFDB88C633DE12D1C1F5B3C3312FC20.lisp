;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "system")
;(require "mapping")
;(require "types")
;(require "chatter")
;(require "options")
;(provide "generic")

(defun mcvs-generic (cvs-command cvs-options command-options command-args 
		     files &key need-sync-before need-sync-after 
		     need-update-after global-if-empty-file-list 
		     no-invoke-cvs)
  (when (and *metaonly-option* files)
    (error "cannot specify both --metaonly option and file arguments."))
  (in-sandbox-root-dir
    (let (files-to-process
	  (filemap (mapping-read *mcvs-map-local*))
	  (do-meta-files (and (or *metaonly-option* *meta-option*)
			      (not *nometa-option*)
			      (or files
				  (not global-if-empty-file-list)
				  *metaonly-option*))))

      (unless *metaonly-option*
	(chatter-debug "Preparing file list.~%")

	(cond
	  ((and (null files)
		global-if-empty-file-list
		*nometa-option*)
	     (setf files-to-process filemap))
	  ((and (null files)
		(not global-if-empty-file-list))
	     (setf files-to-process 
		   (mapping-prefix-matches filemap
					   (sandbox-translate-path "."))))
	  (files
	     (dolist (file files)
	       (can-restart-here ("Continue preparing file list.")
		 (let* ((full-name (sandbox-translate-path file))
			(abs-name (canonicalize-path
				    (real-to-abstract-path full-name)))
			(entries (mapping-prefix-matches filemap abs-name)))
		   (if (not entries)
		     (error "~a is not known to Meta-CVS." full-name)
		     (setf files-to-process (nconc files-to-process 
						   entries)))))))))

      (setf files-to-process (mapping-extract-kind files-to-process :file))

      (when (or files-to-process
		do-meta-files
		global-if-empty-file-list)
	(when need-sync-before
	  (chatter-debug "Synchronizing.~%")
	  (mapping-synchronize :filemap files-to-process
			       :direction :left))
	(unless no-invoke-cvs
	  (current-dir-restore 
	    (chdir *mcvs-dir*) 
	    (chatter-debug "Invoking CVS.~%")
	    (execute-program-xargs `("cvs" ,@(format-opt cvs-options)
				     ,cvs-command ,@(format-opt command-options)
				     ,@command-args)
				   `(,@(when do-meta-files
					 (let (metas)
					   (when (exists ".cvsignore")
					     (push ".cvsignore" metas))
					   (when (exists *mcvs-types-name*)
					     (push *mcvs-types-name* metas))
					   (cons *mcvs-map-name* metas)))
				     ,@(mapcar #'(lambda (x) 
						   (basename 
						     (mapping-entry-id x))) 
					       files-to-process)))))
	(when (and do-meta-files need-update-after)
	  (chatter-debug "Updating file structure.~%")
	  (mapping-update))
	(when need-sync-after
	  (chatter-debug "Synchronizing again.~%")
	  (mapping-synchronize :filemap files-to-process
			       :direction :right))))
  (values)))

(defun mcvs-commit-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "commit" cvs-options cvs-command-options nil mcvs-args 
		:need-sync-before t
		:need-sync-after t
		:global-if-empty-file-list t))

(defun mcvs-diff-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "diff" cvs-options cvs-command-options nil mcvs-args
		:need-sync-before t))

(defun mcvs-tag-wrapper (cvs-options cvs-command-options mcvs-args)
  (if (null mcvs-args)
    (error "specify tag optionally followed by files."))
  (mcvs-generic "tag" cvs-options 
		cvs-command-options (list (first mcvs-args)) (rest mcvs-args)
		:global-if-empty-file-list t))

(defun mcvs-log-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "log" cvs-options cvs-command-options nil mcvs-args))

(defun mcvs-status-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "status" cvs-options cvs-command-options nil mcvs-args
		:need-sync-before t))

(defun mcvs-annotate-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "annotate" cvs-options cvs-command-options nil mcvs-args))

(defun mcvs-watchers-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "watchers" cvs-options cvs-command-options nil mcvs-args))

(defun mcvs-edit-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "edit" cvs-options cvs-command-options nil mcvs-args
		:need-sync-before t))

(defun mcvs-unedit-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "unedit" cvs-options cvs-command-options nil mcvs-args 
		:need-sync-before t
		:need-sync-after t))

(defun mcvs-editors-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "editors" cvs-options cvs-command-options nil mcvs-args))

(defun mcvs-sync-to-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "" cvs-options cvs-command-options nil mcvs-args
		:need-sync-before t
		:no-invoke-cvs t))

(defun mcvs-sync-from-wrapper (cvs-options cvs-command-options mcvs-args)
  (mcvs-generic "" cvs-options cvs-command-options nil mcvs-args
		:need-sync-after t
		:no-invoke-cvs t))
