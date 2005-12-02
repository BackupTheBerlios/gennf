;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "system")
(require "chatter")
(require "find-bind")
(require "split")
(require "error")
(provide "options")

(defvar *print-usage* nil)
(defvar *nometa-option* nil)
(defvar *meta-option* nil)
(defvar *metaonly-option* nil)
(defvar *dry-run-option* nil)
(defvar *nesting-escape-option* 0)

(defun option-spec-expand (num-args string-list)
  (mapcar #'(lambda (string) (list string num-args))
	  string-list))

(defmacro option-spec (&rest option-specs)
  `(append ,@(mapcar #'(lambda (spec)
			 (destructuring-bind (number word &rest strings) spec
			   (when (not (string= (symbol-name word) "ARG"))
			     (error "OPTIONS: word \"ARG\" expected."))
			   `(option-spec-expand ,number ',strings)))
		     option-specs)))

(defmacro define-option-constant (var &rest option-specs)
  `(defconstant ,var (option-spec ,@option-specs)))

(defun parse-opt (arguments option-spec)
  (flet ((process-option (arg)
	   (let* ((split-opt (split-fields arg #(#\=)))
		  (opt-name (first split-opt))
		  (opt-arg (second split-opt))
		  (spec (find opt-name option-spec 
			      :test #'string= 
			      :key #'first)))
	     (when (null spec)
	       (error "unknown option ~a." opt-name))
	     (when opt-arg
	       (push opt-arg arguments))
	     (let ((num-req-params (second spec))
		   (opt-args ()))
	       (dotimes (i num-req-params)
		 (let ((opt-arg (pop arguments)))
		   (when (null opt-arg)
		     (error "option ~a requires ~a parameter~:p." 
			    opt-name num-req-params))
		   (push opt-arg opt-args)))
	       (cons opt-name (nreverse opt-args))))))
    (let ((parsed-options ()))
      (loop
	(if (null arguments)
	  (return))
	(let ((arg (pop arguments)))
	  (cond 
	    ((string= arg "--")
	       (return))
	    ((and (> (length arg) 2) (string= (subseq arg 0 2) "--"))
	       (push (process-option (subseq arg 2)) parsed-options))
	    ((and (> (length arg) 1) (char= (char arg 0) #\-))
	       (let ((num-chars (- (length arg) 1))
		     (last-iter (- (length arg) 2)))
		 (dotimes (i num-chars)
		   (let ((option (subseq arg (+ i 1) (+ i 2)))
			 (arg (subseq arg (+ i 2))))
		     (when (< i last-iter)
		       (push arg arguments))
		     (let ((result (process-option option)))
		       (push result parsed-options)
		       (when (and (second result)
				  (/= i (- (length arg) 2)))
			 (return))
		       (when (< i last-iter)
			 (pop arguments)))))))
	    (t (push arg arguments) 
	       (return)))))
      (values (nreverse parsed-options) arguments))))
		  
(defun format-opt (options)
"Convert list of options as produced by parse-opt back into a list
of strings."
  (mapcan #'(lambda (option-list)
	      (let ((option (first option-list)) 
		    (arg (rest option-list)))
		(if (> (length option) 1)
		  (cons (format nil "--~a" option) arg)
		  (if (= (length arg) 1)
		    (list (format nil "-~a~a" option (first arg)))
		    (cons (format nil "-~a" option) arg))))) 
	  options))

(defun filter-mcvs-options (opts)
"Processes and removes any Meta-CVS-specific options."
  (find-bind (:test #'string= :key #'first)
	     (remainder (meta "meta")
			(metaonly "metaonly")
			(nometa "nometa")
			(ec "error-continue") 
			(et "error-terminate")
			(nesting-escape "up")
		        (debug "debug"))
	     opts
    (when (and meta nometa)
      (error "cannot specify both --nometa and --meta"))
    (when (and metaonly nometa)
      (error "cannot specify both --nometa and --metaonly"))
    (setf *meta-option* meta)
    (setf *metaonly-option* metaonly)
    (setf *nometa-option* nometa)
    (when nesting-escape
      (unless (setf *nesting-escape-option* 
		    (parse-integer (second nesting-escape)
				   :junk-allowed t))
	(error "--up option takes integer argument"))
      (unless (>= *nesting-escape-option* 0)
	(error "--up argument must be nonnegative")))
    (when debug
      (setf *mcvs-chatter-level* *mcvs-debug*))
    (cond
      (ec (setf *mcvs-error-treatment* :continue))
      (et (setf *mcvs-error-treatment* :terminate)))
    remainder))

(defun process-cvs-options (opts)
"Take care of any CVS options that must also be interpreted by Meta-CVS."
  (find-bind (:test #'string= :key #'first) 
	     ((help-long "help") (help "H") (quiet "q")
	      (very-quiet "Q") (version "v") (version-long "version")
	      (editor "e") (interpret-file "i") (dry-run "n"))
	     opts
    (when (or help-long help)
      (setf *print-usage* t))
    (when (or version version-long)
      (let* ((vers (split-words "$Name:  $" "$:- "))
	     (major (third vers))
	     (minor (fourth vers))
	     (patch (fifth vers)))
	(if (and major minor patch)
	  (format t "Meta-CVS version ~a.~a.~a Copyright 2004 Kaz Kylheku~%"
		  major minor patch)
	  (format t "Meta-CVS unknown version Copyright 2004 Kaz Kylheku~%"))
	(throw 'mcvs-terminate nil)))
    (when editor
      (setf *mcvs-editor* (second editor)))
    (cond
      (very-quiet (setf *mcvs-chatter-level* *mcvs-silent*))
      (quiet (setf *mcvs-chatter-level* *mcvs-terse*)))
    (when dry-run
      (setf *dry-run-option* t))
    (when interpret-file
      (load (second interpret-file))
      (throw 'mcvs-terminate nil)))
  opts)

(defun filter-global-options (opts)
  (process-cvs-options (filter-mcvs-options opts)))

(defmacro honor-dry-run (vars &rest forms)
  `(cond
     (*dry-run-option*
       (chatter-debug 
	 "Because of -n option, not executing ~s with bindings ~s.~%"
	 ',forms
	 (list ,@(mapcar #'(lambda (var) `(list ',var ,var)) vars))))
     (t ,@forms)))
