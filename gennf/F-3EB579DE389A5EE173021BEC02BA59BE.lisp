;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "dirwalk")
(require "mapping")

(defun mcvs-restore ()
  (in-sandbox-root-dir
    (let* ((filemap (mapping-read *mcvs-map* :sanity-check t))
	   (removed-files (mapping-removed-files filemap)))
      (dolist (removed removed-files)
	(push (make-mapping-entry :kind :file
				  :id removed
				  :path (path-cat (real-to-abstract-path 
						    "lost+found")
						  (basename removed))
				  :executable (executable-p removed))
	      filemap))
      (mapping-write filemap *mcvs-map* :sort-map t)
      (mapping-update))))

(defun mcvs-restore-wrapper (global-options command-options args)
  (declare (ignore global-options command-options))
  (when args
    (error "no arguments permitted."))
  (mcvs-restore))
