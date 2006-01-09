;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "system")
;(provide "sync")

(defun synchronize-files (left-file right-file should-be-executable
			  &key (direction :either))
"Ensure that the two files have the same contents, using any means,
including the possibility of making them hard links to the same object.
If they are already the same object, nothing happens and the symbol
:same is returned. Otherwise the newer one prevails, and clobbers the older
one; the symbols :left or :right are returned to indicate which prevailed. If
one of them doesn't exist, then it is created. If neither exists, nothing
happens, and NIL is returned. If either file is actually a directory, 
:dir is returned"
  (flet ((exec-check (file-info)
	   (if should-be-executable
	     (make-executable file-info)
	     (make-non-executable file-info))))
    (let ((left (exists left-file))
	  (right (exists right-file)))
      (cond
	((not (or left right)) 
	  nil)
	((or (directory-p left) (directory-p right))
	  :dir)
	((not right)
	  (honor-dry-run (left-file right-file)
	    (exec-check left)
	    (or (unless (eq direction :left)
		  (ensure-directories-exist right-file)
		  (link left-file right-file)
		  :left)
		:no-sync)))
	((not left)
	  (honor-dry-run (left-file right-file)
	    (exec-check right)
	    ;; Special case: do not re-create files missing in 
	    ;; CVS sandbox! Either someone tampered with the sandbox,
	    ;; in which case we just let CVS resurrect the file,
	    ;; and a subsequent sync will properly have the clean-copy
	    ;; semantics, propagating the clean copy to the tree.
	    ;; Or else CVS itself made the file disappear, in which 
	    ;; case if we restore it, CVS will later complain that the file
	    ;; is ``in the way''!
	    :no-sync))
	((same-file-p right left) 
	  (honor-dry-run (right-file)
	    (exec-check right))
	  :same)
	((older-p left right)
	  (honor-dry-run (left-file right-file)
	    (exec-check right)
	    (or (unless (eq direction :right)
		  (unlink left-file)
		  (link right-file left-file)
		  :right)
		:no-sync)))
	((older-p right left)
	  (honor-dry-run (left-file right-file)
	    (exec-check left)
	    (or (unless (eq direction :left)
		  (unlink right-file)
		  (link left-file right-file)
		  :left)
		:no-sync)))
	(t
	  (restart-case
	    (ecase direction
	      ((:right) (invoke-restart :choose-left))
	      ((:left) (invoke-restart :choose-right))
	      ((:either) (error "~a and ~a have the same modification time."
				left-file right-file)))
	    (:choose-left () :report (lambda (s)
				       (format s "take ~a; clobber ~a." 
					       left-file right-file))
	      (honor-dry-run (left-file right-file)
		(unlink right-file)
		(link left-file right-file)
		(exec-check left))
	      :left)
	    (:choose-right () :report (lambda (s)
					(format s "take ~a; clobber ~a." 
						right-file left-file))
	      (honor-dry-run (left-file right-file)
		(unlink left-file)
		(link right-file left-file)
		(exec-check right))
	      :right)))))))