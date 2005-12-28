;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "system")
;(require "mapping")
;(require "chatter")
;(provide "prop")

(defun mcvs-prop (prop-options files)
  (in-sandbox-root-dir
    (let (entries-to-process
	  (filemap (mapping-read *mcvs-map*)))
      (chatter-debug "Preparing file list.~%")

      (if (null files)
	(setf entries-to-process 
	      (mapping-prefix-matches filemap
				      (sandbox-translate-path ".")))
	(dolist (file files)
	  (can-restart-here ("Continue preparing file list.")
	    (let* ((full-name (sandbox-translate-path file))
		   (abs-name (canonicalize-path 
			       (real-to-abstract-path full-name)))
		   (entries (mapping-prefix-matches filemap abs-name)))
	      (if (not entries)
		(error "~a is not known to Meta-CVS." full-name)
		(setf entries-to-process (nconc entries-to-process entries)))))))

      (when (and entries-to-process prop-options)
	;; do the property update
	(chatter-debug "Updating properties.~%")
	(dolist (entry entries-to-process)
	  (with-slots (raw-plist) entry
	    (loop for (option prop-name value) in prop-options do
	      (let ((indicator (intern (string-upcase prop-name) "KEYWORD")))
		(cond
		  ((string= option "set")
		     (setf (getf raw-plist indicator) t))
		  ((string= option "clear")
		     (setf (getf raw-plist indicator) nil))
		  ((string= option "value")
		     (setf (getf raw-plist indicator) (read-from-string value)))
		  ((string= option "remove")
		     (remf raw-plist indicator)))
		(mapping-entry-parse-plist entry)))))
	(chatter-debug "Writing out map.~%")
	(mapping-write filemap *mcvs-map*)
	;; propagate changes to local map.
	(chatter-debug "Updating file structure.~%")
	(mapping-update)
	;; propagate permission changes to files.
	(chatter-debug "Synchronizing.~%")
	(mapping-synchronize))))
  (values))

(defun mcvs-prop-wrapper (mcvs-opts command-opts command-args)
  (declare (ignore mcvs-opts))
  (mcvs-prop command-opts command-args))
