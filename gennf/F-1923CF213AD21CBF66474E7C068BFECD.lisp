;;; This source file is part of the Meta-CVS program, 
;;; which is distributed under the GNU license.
;;; Copyright 2002 Kaz Kylheku

(in-package :gennf)

;(provide "split")

(defun split-fields (in-string delim-char-bag)
"Split string into rigid fields based on delimiter characters. 
Each individual delimiter character separates two fields.
Example:  (split-fields \":c:#a\" \":#\") ==> (\"\" \"c\" \"\" \"a\")"
  (let (list (tok-start 0))
    (if (= (length delim-char-bag) 1)
      (let ((delim (aref delim-char-bag 0)))
	(dotimes (index (length in-string)) 
	  (when (char= delim (char in-string index))
	    (push (subseq in-string tok-start index) list)
	    (setf tok-start (1+ index)))))
      (dotimes (index (length in-string)) 
	(when (find (char in-string index) delim-char-bag :test #'char=)
	    (push (subseq in-string tok-start index) list)
	    (setf tok-start (1+ index)))))
    (push (subseq in-string tok-start) list)
    (nreverse list)))

(defun split-words (in-string delim-char-string)
"Munge sequences of delimiter characters. The pieces
in between are returned as a list of strings.
Example:  (split-words \" a b cde f \"  \" \") ==> (\"a\" \"b\" \"cde\" \"f\")"
  (let (list (token "") (state :parsing-delim))
    (dotimes (i (length in-string))
      (let ((ch (aref in-string i)))
        (if (not (find ch delim-char-string))
          (progn (setf token (format nil "~a~a" token ch))
                 (setf state :parsing-token))
          (when (eq state :parsing-token)
            (push token list)
            (setf token "")
            (setf state :parsing-delim)))))
    (when (not (equal token ""))
      (push token list))
    (nreverse list)))

#| (time (dotimes (x 1000) (split-fields "aaa:bb:ccc:ddd:eee" ":"))) |#

