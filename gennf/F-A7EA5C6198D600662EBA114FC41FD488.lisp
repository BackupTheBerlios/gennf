;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(require "split")
(provide "posix")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *up-dir* "..")
  (defconstant *this-dir* ".")
  (defconstant *path-sep* "/"))

(defvar *mcvs-editor* nil)

(defconstant *argument-limit* (* 64 1024))

(defun canonicalize-path (path)
"Simplifies a POSIX path by eliminating . components, splicing out as many ..
components as possible, and condensing multiple slashes. A trailing slash is
guaranteed to be preserved, if it follows something that could be a file or
directory.  Two values are returned, the simplified path and a boolean value
which is true if there are any .. components that could not be spliced out."
  (let ((split-path (split-fields path "/"))
        uncanceled-up)

    ;; First, if the path has at least two components,
    ;; replace the first empty one with the symbol :root
    ;; and the last empty one with :dir. These indicate a
    ;; leading and trailing /
    (when (and (> (length split-path) 1))
      (when (string= (first split-path) "")
        (setf (first split-path) :root))
      (when (string= (first (last split-path)) "")
        (setf (first (last split-path)) :dir)))

    ;; Next, squash out all of the . and empty components,
    ;; and replace .. components with :up symbol.
    (setf split-path (mapcan #'(lambda (item)
				 (cond
				   ((string= item "") nil)
				   ((string= item ".") nil)
				   ((string= item "..") (list :up))
				   (t (list item)))) 
			     split-path))
    (let (folded-path)
      ;; Now, we use a pushdown automaton to reduce the .. paths
      ;; The remaining stack is the reversed path.
      (dolist (item split-path)
        (case item
	  ((:up)
	    (case (first folded-path)
	      ((:root)) ;; do nothing
	      ((:up nil) (push item folded-path) (setf uncanceled-up t))
	      (otherwise (pop folded-path))))
	  ((:dir)
	    (case (first folded-path)
	      ((:root :up nil))
	      (otherwise (push (concatenate 'string (pop folded-path) "/")
	                       folded-path))))
          (otherwise
	    (push item folded-path))))
      (setf split-path (nreverse folded-path)))

    ;; If there are at least two components, remove a leading :root
    ;; and add a / to the first component. If there are 0 components
    ;; add a "." component.
    (if (zerop (length split-path))
      (push "." split-path)
      (when (eq (first split-path) :root)
	(pop split-path)
	(push (concatenate 'string "/" (or (pop split-path) "")) split-path)))

    ;; Map remaining symbols back to strings
    (setf split-path (mapcar #'(lambda (item)
				 (case item
				   ((:up) "..")
				   (otherwise item))) split-path))

    ;; Convert back to text
    (values (reduce #'(lambda (x y) (concatenate 'string x "/" y)) split-path)
            uncanceled-up)))

(defun basename (path)
"Splits the path into base name and directory, returned as two values.
If the path is / then . and / are returned. The rightmost slash is
used to determine the split between the path and the base name. If there
is a rightmost slash, then everything up to but not including that slash is
returned as the directory (second) value, and everything to the right is 
returned as the base name (first) value. If there is no rightmost slash,
then the directory is returned as NIL, and the path is the entire base name.
If the path has a trailing slash, then that trailing slash is part of the base
name, and does not count as the rightmost slash."
  (let* ((pos1 (position #\/ path :from-end t))
         (pos2 (position #\/ path :end pos1 :from-end t)))
    (cond
      ((string= path "/") 
        (values "." "/"))
      ((null pos1) 
        (values path nil))
      ((= (1+ pos1) (length path))
        (if (null pos2)
          (values path nil)
	  (values (subseq path (1+ pos2)) (subseq path 0 pos2))))
      (t
        (values (subseq path (1+ pos1)) (subseq path 0 pos1))))))

(defun suffix (path &optional (separator-char #\.))
  (multiple-value-bind (name dir) 
		       (basename path)
    (let ((pos (position separator-char name)))
      (cond
        ((eql pos 0)
	   (values nil name dir))
	(pos
	   (values (subseq name (1+ pos)) (subseq name 0 pos) dir))
	(t (values nil name dir))))))

(declaim (inline path-equal))
(defun path-equal (p1 p2)
  (string= p1 p2))

(defun path-prefix-equal (shorter longer)
  (let ((ls (length shorter)) (ll (length longer)))
    (cond
      ((> ls ll) nil)
      ((not (string= shorter longer :end2 ls)) nil)
      ((= ls ll) t)
      ((and (> ls 0) 
	    (char-equal (char shorter (1- ls)) #\/)
	    (char-equal (char longer (1- ls))) #\/) t)
      ((char-equal (char longer ls) #\/) t)
      (t nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun path-cat (first-component &rest components)
    (reduce #'(lambda (x y) (format nil "~a/~a" x y)) components
	    :initial-value first-component)))

(defun path-absolute-p (path)
  (unless (zerop (length path))
    (char= (char path 0) #\/)))

(defun parse-posix-namestring (path)
  (let ((split-path (split-fields path "/")))
    (let ((dir (butlast split-path))
	  (name (first (last split-path))))
      (apply #'make-pathname 
	     `(,@(when dir 
		   `(:directory ,(if (string= "" (first dir))
				   `(:absolute ,@(rest dir))
				   `(:relative ,@dir))))
	       ,@(when name
		  `(:name ,name)))))))

(defun arglist-to-command-string (arglist)
"Convert list of strings, assumed to be an argument vector, into
a single command string that can be submitted to a POSIX command 
interpreter. This requires escaping of all shell meta-characters."
  (let ((command (make-array '(1024)
			     :element-type 'character
			     :adjustable t
			     :fill-pointer 0)))
    (dolist (arg arglist command)
      (dotimes (i (length arg))
	(let ((ch (char arg i)))
	  (when (find ch #(#\' #\" #\* #\[ #\] #\? 
			   #\$ #\{ #\} #\" #\space #\tab
			   #\( #\) #\< #\> #\| #\; #\&))
	    (vector-push-extend #\\ command))
	  (vector-push-extend ch command)))
	(vector-push-extend #\space command))))

(defun execute-program-xargs (fixed-args &optional extra-args fixed-trail-args)
  (let* ((fixed-size (reduce #'(lambda (x y)
				 (+ x (length y) 1))
			     (append fixed-args fixed-trail-args)
			     :initial-value 0))
	 (size fixed-size))
    (if extra-args
      (let ((chopped-arg ())
	    (combined-status t))
	(dolist (arg extra-args)
	  (push arg chopped-arg)
	  (when (> (incf size (1+ (length arg))) *argument-limit*)
	    (setf combined-status 
		  (and combined-status
		       (execute-program (append fixed-args 
						(nreverse chopped-arg)
						fixed-trail-args))))
	    (setf chopped-arg nil)
	    (setf size fixed-size)))
	(when chopped-arg
	  (execute-program (append fixed-args (nreverse chopped-arg)
				   fixed-trail-args)))
	combined-status)
      (execute-program (append fixed-args fixed-trail-args)))))

(defun invoke-editor-on (name)
  (let ((editor (or *mcvs-editor* 
		    (env-lookup "CVSEDITOR")
		    (env-lookup "VISUAL")
		    (env-lookup "EDITOR" "vi"))))
    (execute-program `(,editor ,name))))
