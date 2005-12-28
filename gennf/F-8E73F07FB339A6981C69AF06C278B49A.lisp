;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(require "slot-refs")
;(provide "rcs-utils")

(defstruct rcs-token
  (type)
  (lexeme))

(defstruct rcs-token-stream
  (stream)
  (pushback-stack))

(defgeneric rcs-read-token (stream))
(defgeneric rcs-peek-token (stream tok))

(defstruct rcs-admin
  (head)
  (branch)
  (access-list)
  (symbols)
  (locks)
  (locks-strict)
  (comment)
  (expand)
  (newphrases))

(defstruct rcs-delta
  (version)
  (date)
  (author)
  (state)
  (branches)
  (next)
  (newphrases))

(defstruct rcs-file
  (admin)
  (deltas)
  (delta-hash))

(defun rcs-special-p (ch)
  (or (char= ch #\$) (char= ch #\,) (char= ch #\.) 
      (char= ch #\;) (char= ch #\:) (char= ch #\@)))

(defun rcs-extract-id-sym-or-num (stream)
  (let (contains-dot contains-idchar)
    (let ((lexeme (with-output-to-string (ss)
		    (loop
		      (let ((ch (peek-char nil stream)))
			(cond
			  ((char= ch #\.)
			     (setf contains-dot t)
			     (write-char ch ss))
			  ((digit-char-p ch)
			     (write-char ch ss))
			  ((or (rcs-special-p ch) (char= ch #\space)
			     (not (graphic-char-p ch)))
			     (return))
			  (t (setf contains-idchar t)
			     (write-char ch ss)))
			(read-char stream))))))
    (make-rcs-token :type (cond
			    ((and contains-dot contains-idchar) :id)
			    (contains-idchar :sym)
			    (t :num))
		    :lexeme lexeme))))

(defun rcs-extract-string (stream)
  (read-char stream)
  (make-rcs-token
    :type :string
    :lexeme
    (with-output-to-string (ss)
      (let ((state :initial))
	(loop
	  (let ((ch (peek-char nil stream)))
	    (case state
	      ((:initial)
		 (if (char= ch #\@)
		   (setf state :atsign)
		   (write-char ch ss)))
	      (otherwise
		 (if (char= ch #\@)
		   (progn
		     (write-char ch ss)
		     (setf state :initial))
		   (return))))
	    (read-char stream)))))))

(defmethod rcs-read-token ((stream stream))
  (handler-bind ((end-of-file #'(lambda (condition) 
				  (declare (ignore condition))
				  (return-from rcs-read-token 
					       (load-time-value 
						 (make-rcs-token :type :eof
								 :lexeme ""))))))
    (loop
      (let ((ch (peek-char nil stream)))
	(cond
	  ((digit-char-p ch)
	     (return (rcs-extract-id-sym-or-num stream)))
	  ((char= ch #\@)
	     (return (rcs-extract-string stream)))
	  ((rcs-special-p ch) 
	     (read-char stream)
	     (return (make-rcs-token :type :special :lexeme ch)))
	  ((and (graphic-char-p ch) (not (char= ch #\space)))
	     (return (rcs-extract-id-sym-or-num stream)))
	  (t (read-char stream)))))))

(defmethod rcs-read-token ((stream t))
  (rcs-read-token *standard-input*))

(defmethod rcs-read-token ((rts rcs-token-stream))
  (with-slots (stream pushback-stack) rts
    (cond
      ((pop pushback-stack))
      (t (rcs-read-token stream)))))

(defmethod rcs-pushback-token ((rts rcs-token-stream) (tok rcs-token))
  (push tok (slot-value rts 'pushback-stack)))

(defun rcs-match-optional (stream type-match &optional lexeme-match)
  (let ((token (rcs-read-token stream)))
    (with-slots (type lexeme) token
      (cond
	((and lexeme-match
	      (not (and (eq type type-match) (string= lexeme lexeme-match))))
	   (rcs-pushback-token stream token)
	   nil)
	((not (eq type type-match))
	   (rcs-pushback-token stream token)
	   nil)
	(t token)))))

(defun rcs-match-token (stream type-match &optional lexeme-match)
  (let ((token (rcs-read-token stream)))
    (with-slots (type lexeme) token
      (if lexeme-match
	(when (not (and (eq type type-match) (string= lexeme lexeme-match)))
	  (error "rcs-parse: expecting token \"~a\" of type ~a, not \"~a\" of type ~a." 
		 lexeme-match type-match lexeme type))
	(when (not (eq type type-match))
	  (error "rcs-parse: expecting token of type ~a, not ~a." type-match type)))
      token)))

(defun rcs-parse-newphrases (stream)
  (let (token newphrases)
    (with-slot-refs (lexeme) token
      (loop
	(if (setf token (or (rcs-match-optional stream :sym)
			    (rcs-match-optional stream :id)))
	  (let ((phrase (list lexeme)))
	    (loop
	      (cond
		((setf token (or (rcs-match-optional stream :sym)
				 (rcs-match-optional stream :id)
				 (rcs-match-optional stream :num)
				 (rcs-match-optional stream :string)))
		   (push lexeme phrase))
		((setf token (rcs-match-optional stream :special))
		   (if (char= lexeme #\:)
		     (push lexeme phrase)
		     (progn
		       (rcs-pushback-token stream token)
		       (push (nreverse phrase) newphrases)
		       (return))))
		(t (push (nreverse phrase) newphrases)
		    (return))))
	    (rcs-match-token stream :special #\;))
	  (return))))
    (nreverse newphrases)))

(defun rcs-parse-admin (stream)
  (let ((admin (make-rcs-admin))
	token)
    (with-multi-slot-refs ((head branch access-list symbols locks locks-strict 
			    comment expand newphrases) admin
			   (lexeme) token)
      ;; head { num } ;
      (rcs-match-token stream :sym "head")
      (setf token (rcs-match-optional stream :num))
      (when token
	(setf head lexeme))
      (rcs-match-token stream :special #\;)

      ;; { branch { num } ; }
      (when (rcs-match-optional stream :sym "branch")
	(setf token (rcs-match-optional stream :num))
	(when token
	  (setf branch lexeme))
	(rcs-match-token stream :special #\;))

      ;; access { id } * ;
      (rcs-match-token stream :sym "access")
      (loop
	(setf token (or (rcs-match-optional stream :sym)
			(rcs-match-optional stream :id)))
	(if token
	  (push lexeme access-list)
	  (return)))
      (nreverse access-list)
      (rcs-match-token stream :special #\;)

      ;; symbols { sym : num }* ;
      (rcs-match-token stream :sym "symbols")
      (loop
	(setf token (rcs-match-optional stream :sym))
	(cond
	  (token
	     (let ((symbol lexeme))
	       (rcs-match-token stream :special #\:)
	       (setf token (rcs-match-token stream :num))
	       (push (list symbol lexeme) symbols)))
	  (t (return))))
      (nreverse symbols)
      (rcs-match-token stream :special #\;)

      ;; locks { id : num }* ; { strict ; }
      (rcs-match-token stream :sym "locks")
      (loop
	(setf token (or (rcs-match-optional stream :sym)
			(rcs-match-optional stream :id)))
	(cond
	  (token
	     (let ((symbol lexeme))
	       (rcs-match-token stream :special #\:)
	       (setf token (rcs-match-token stream :num))
	       (push (list symbol lexeme) locks)))
	  (t (return))))
      (nreverse locks)
      (rcs-match-token stream :special #\;)
      (when (rcs-match-optional stream :sym "strict")
	(setf locks-strict t)
	(rcs-match-token stream :special #\;))

      ;; { comment { string } ; }
      (when (rcs-match-optional stream :sym "comment")
	(setf token (rcs-match-optional stream :string))
	(when token
	  (setf comment lexeme))
	(rcs-match-token stream :special #\;))

      ;; { expand { string } ; }
      (when (rcs-match-optional stream :sym "expand")
	(setf token (rcs-match-optional stream :string))
	(when token
	  (setf expand lexeme))
	(rcs-match-token stream :special #\;))

      ;; { newphrase }*
      (setf newphrases (rcs-parse-newphrases stream))
      admin)))

(defun rcs-parse-delta (stream)
  (let ((delta (make-rcs-delta))
	token)
    (with-multi-slot-refs ((version date author state branches 
			    next newphrases) delta
			   (lexeme) token)
      ;; num
      (setf token (rcs-match-optional stream :num))
      (if (not token)
	(return-from rcs-parse-delta nil))

      (setf version lexeme)

      ;; date num ;
      (rcs-match-token stream :sym "date")
      (setf token (rcs-match-token stream :num))
      (setf date lexeme)
      (rcs-match-token stream :special #\;)

      ;; author id ;
      (rcs-match-token stream :sym "author")
      (setf token (or (rcs-match-optional stream :sym)
		      (rcs-match-token stream :id)))
      (setf author lexeme)
      (rcs-match-token stream :special #\;)

      ;; state { id } ;
      (rcs-match-token stream :sym "state")
      (setf token (or (rcs-match-optional stream :sym)
		      (rcs-match-optional stream :id)))
      (when token
	(setf state lexeme))
      (rcs-match-token stream :special #\;)

      ;; branches { num } * ;
      (rcs-match-token stream :sym "branches")
      (loop
	(let ((token (rcs-match-optional stream :num)))
	  (if token
	    (push lexeme branches)
	    (return (nreverse branches)))))
      (rcs-match-token stream :special #\;)

      ;; next { num } ;
      (rcs-match-token stream :sym "next")
      (setf token (rcs-match-optional stream :num))
      (when token
	(setf next lexeme))
      (rcs-match-token stream :special #\;)

      ;; { newphrase }*
      (when (not (rcs-match-optional stream :sym "desc"))
	(setf newphrases (rcs-parse-newphrases stream)))
      delta)))

(defun rcs-parse-deltas (stream)
  (let (deltas)
    (loop
      (let ((delta (rcs-parse-delta stream)))
	(if delta
	  (push delta deltas)
	  (return (nreverse deltas)))))))

(defun rcs-make-delta-hash (deltas)
  (let ((hash (make-hash-table :test #'equal)))
    (mapc #'(lambda (delta)
	      (setf (gethash (slot-value delta 'next) hash) delta))
	  deltas)
    hash))

(defun rcs-parse (stream)
  "Parse RCS file."
  (let ((token-stream (make-rcs-token-stream :stream stream)))
    ;; We currently just need the admin and delta sections.
    (let ((file (make-rcs-file :admin (rcs-parse-admin token-stream)
			       :deltas (rcs-parse-deltas token-stream))))
      (setf (slot-value file 'delta-hash) 
	    (rcs-make-delta-hash (slot-value file 'deltas)))
      file)))
