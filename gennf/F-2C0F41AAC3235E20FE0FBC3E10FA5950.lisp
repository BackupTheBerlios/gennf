;; sudoku.lisp
;;
;; Part of fd, written by Florian Lorenzen <florenz@cs.tu-berlin.de>
;;
;; Framework to solve Sudoku puzzles with fd.
;; 

(in-package :sudoku)


;;
;; Two example puzzles.
;; 

(defparameter *easy*
  '((1 5 2) (1 7 7)
    (2 1 8) (2 4 3) (2 7 9)
    (3 3 6) (3 6 5) (3 9 1)
    (4 1 4) (4 4 8) (4 5 7) (4 8 2)
    (5 2 9) (5 4 1) (5 8 3)
    (6 2 5) (6 5 6) (6 9 4)
    (7 1 7) (7 4 9) (7 7 5)
    (8 3 1) (8 6 6) (8 9 8)
    (9 3 5) (9 5 4))
  "Easy Sudoku no. 1000 from www.sudoku.org. Takes about 1 second on 1 GHz.")

(defparameter *insane*
  '((1 1 8) (1 4 6) (1 9 2)
    (2 2 4) (2 5 5) (2 8 1)
    (3 4 7) (3 9 3)
    (4 2 9) (4 6 4) (4 9 6)
    (5 1 2) (5 9 8)
    (6 1 7) (6 5 1) (6 8 5)
    (7 1 3) (7 6 9)
    (8 2 1) (8 5 8) (8 8 9)
    (9 1 4) (9 6 2) (9 9 5))
  "Insance Sudoku no. 4000 from www.sudoku.org. Takes about 25
seconds on 1 GHz.")


;;
;; Functions to generate constraints for a given puzzle and solve it.
;;

(defun solve-sudoku (sudoku)
  "Solves the given sudoku."
  (show-solution (fd-solv (generate-constraints sudoku))))

(defun generate-constraints (assignments)
  "Generate the constraint satisfaction problem for the given
Sudoku board. The assignments are the preset fields and given
as a list of triples: (i j v) with i being the row, j being the
column, i=j=1, ..., 9, and v the preset value, v=1, ..., 9."
  (let* ((variables (generate-variables))
	 (constraints (append (alldifferent-row variables)
			      (alldifferent-column variables)
			      (alldifferent-square variables)
			      (generate-domains variables))))
    (dolist (assignment assignments)
      (let ((i (first assignment))
	    (j (second assignment))
	    (value (third assignment)))
	(setf constraints
	      (cons `(fd-eq ,(nth (+ (* (1- i) 9) (1- j)) variables) ,value)
		    constraints))))
    constraints))

(defun generate-domains (variables)
  "Generate the domain constraints for all variables. They
are constrainted to be in the interval 1, ..., 9."
  (let ((constraints ()))
    (dolist (variable variables)
      (push `(fd-in ,variable (1 2 3 4 5 6 7 8 9)) constraints))
    constraints))

(defun generate-variables ()
  "Generate a list of symbols, one for each field in the
Sudoku board. They have names sij with i being the row and
j the column of the field."
  (loop for i from 1 to 9
	append (loop for j from 1 to 9
		     collect (make-symbol (format nil "s~S~S" i j)))))

(defun alldifferent-row (variables)
  "Return the all different constraints for the rows."
  (when variables
    (append (alldifferent (subseq variables 0 9))
	    (alldifferent-row (nthcdr 9 variables)))))

(defun alldifferent-column (variables)
  "Return the all different constraints for the columns."
  (let ((result ()))
    (dotimes (start 9)
      (setf result
	    (append (alldifferent (every-nth 9 variables start)) result)))
    result))

(defun alldifferent-square (variables)
  "Return the all different constraints for the squares."
  (let ((result ()))
    (dotimes (i 3)
      (dotimes (j 3)
	(let ((square ()))
	  (dotimes (k 3)
	    (dotimes (l 3)
	      ;; Index foo. Consider variables as 9x9 matrix.
	      ;; i, j count the submatrices, k, l are submatrix
	      ;; indices.
	      (push (nth (+ (* (+ (* i 3) k) 9) (* j 3) l)
			 variables) square)))
	  (setf result (append (alldifferent square) result)))))
    result))

(defun every-nth (n list &optional (start 0))
  "Return the elemnt no. start and all subsequent elements
i*n apart, i=1, 2, 3, ..."
  (when list
    (let ((tail (nthcdr start list)))
      (append (list (first tail)) (every-nth n (nthcdr n tail))))))
