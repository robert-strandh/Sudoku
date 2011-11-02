(defpackage :sudoku
  (:use :clim :clim-lisp)
  (:export #:sudoku))

(in-package :sudoku)

(define-application-frame sudoku ()
  ((orig-grid :initarg :grid)
   (grid :reader grid))
  (:panes (app :application
	       :width 1000 :height 500
	       :display-function 'display-grid)
	  (int :interactor :width 1000 :height 50))
  (:layouts (default (vertically () app int))))

(defun init-grid (frame)
  (with-slots (orig-grid grid) frame
    (setf grid (make-array '(9 9)))
    (loop for r from 0 below 9
	  do (loop for c from 0 below 9
		   do (setf (aref grid r c)
			    (if (zerop (aref orig-grid r c))
				'(1 2 3 4 5 6 7 8 9)
				(list (aref orig-grid r c))))))))

(defmethod initialize-instance :after ((frame sudoku) &rest args)
  (declare (ignore args))
  (init-grid frame))

;;; when nil, display a blank in a cell that has more than one
;;; possibility, and an asterisk in a cell that has no possibilities.
;;; when non-nil, display all the possibilities of each cell
(defparameter *exploded-view* nil)

(defun draw-cell (pane grid row col)
  (let ((contents (aref grid row col)))
    (formatting-cell (pane)
      (surrounding-output-with-border (pane)
	(with-output-as-presentation
	    (pane (+ (* row 9) col) 'integer)  
	  (cond (*exploded-view*
		 (loop for elem in contents
		       do (princ elem pane)))
		((null contents)
		 (princ "*" pane))
		((null (cdr contents))
		 (princ (car contents) pane))
		(t (princ " " pane))))))))

(defun draw-3x3 (pane grid row col)
  (surrounding-output-with-border (pane)
    (formatting-table (pane :equalize-column-widths t)
      (loop for r from row
	    repeat 3
	    do (formatting-row (pane)
		 (loop for c from col
		       repeat 3
		       do (draw-cell pane grid r c)))))))
			      

(defun display-grid (frame pane)
  (surrounding-output-with-border (pane)
    (formatting-table (pane :equalize-column-widths t)
      (loop for i from 0 by 3
	    repeat 3
	    do (formatting-row (pane)
		 (loop for j from 0 by 3
		       repeat 3
		       do (formatting-cell (pane)
			    (draw-3x3 pane (grid frame) i j))))))))

;;; for a given row, if there is a column with only one possible
;;; number, then remove that number as a possibility from the other
;;; columns in the row
(defun propagate-row (grid row)
  (loop for col from 0 below 9
	do (when (= (length (aref grid row col)) 1)
	     (loop with entry = (car (aref grid row col))
		   for c from 0 below 9
		   do (unless (= c col)
			(when (member entry (aref grid row c))
			  (setf (aref grid row c)
				(remove entry (aref grid row c)))))))))

;;; for a given column, if there is a row with only one possible
;;; number, then remove that number as a possibility from the other
;;; rows of the column
(defun propagate-col (grid col)
  (loop for row from 0 below 9
	do (when (= (length (aref grid row col)) 1)
	     (loop with entry = (car (aref grid row col))
		   for r from 0 below 9
		   do (unless (= r row)
			(when (member entry (aref grid r col))
			  (setf (aref grid r col)
				(remove entry (aref grid r col)))))))))

;;; for a given 3x3 square, if there is a cell with only one possible
;;; number, then remove that number as a possibility from the other
;;; cells of the square
(defun propagate-3x3 (grid row col)
  (loop for r from row
	repeat 3
	do (loop for c from col
		 repeat 3
		 do (when (= (length (aref grid r c)) 1)
		      (loop with entry = (car (aref grid r c))
			    for rr from row
			    repeat 3
			    do (loop for cc from col
				     repeat 3
				     do (unless (and (= rr r) (= cc c))
					  (when (member entry (aref grid rr cc))
					    (setf (aref grid rr cc)
						  (remove entry (aref grid rr cc)))))))))))
						     

;;; systematically propagate all single-number cells to other cells in
;;; the same row, the same column, and the same 3x3 square.  This
;;; could be done better than just repeating alot of times, but why bother. 
(defun propagate-all (grid)
  (loop repeat 1000
	do (progn (loop for row from 0 below 9
			do (propagate-row grid row))
		  (loop for col from 0 below 9
			do (propagate-col grid col))
		  (loop for row from 0 by 3
			repeat 3
			do (loop for col from 0 by 3
				 repeat 3
				 do (propagate-3x3 grid row col))))))

;;; for a given row, if there is a number that is one of the
;;; possibilities of exactly one column in that row, then assign that
;;; number to that cell
(defun try-impose-row (grid row)
  (loop for i from 1 to 9
	do (let ((count (loop for col from 0 below 9
			      count (member i (aref grid row col)))))
	     (when (= count 1)
	       (loop for col from 0 below 9
		     do (when (member i (aref grid row col))
			  (setf (aref grid row col) (list i))))))))		 

;;; for a given column, if there is a number that is one of the
;;; possibilities of exactly one row in that column, then assign that
;;; number to that cell
(defun try-impose-col (grid col)
  (loop for i from 1 to 9
	do (let ((count (loop for row from 0 below 9
			      count (member i (aref grid row col)))))
	     (when (= count 1)
	       (loop for row from 0 below 9
		     do (when (member i (aref grid row col))
			  (setf (aref grid row col) (list i))))))))		 

;;; for a given 3x3 square, if there is a number that is one of the
;;; possibilities of exactly one cell of that square, then assign that
;;; number to that cell
(defun try-impose-3x3 (grid row col)
  (loop for i from 1 to 9
	do (let ((count (loop for r from row
			      repeat 3
			      sum (loop for c from col
					repeat 3
					count (member i (aref grid r c))))))
	     (when (= count 1)
	       (loop for r from row
		     repeat 3
		     do (loop for c from col
			      repeat 3
			      do (when (member i (aref grid r c))
				   (setf (aref grid r c) (list i)))))))))

(defun try-impose-all (grid)
  (loop repeat 1000
	do (progn (loop for row from 0 below 9
			do (try-impose-row grid row))
		  (loop for col from 0 below 9
			do (try-impose-col grid col))
		  (loop for row from 0 by 3
			repeat 3
			do (loop for col from 0 by 3
				 repeat 3
				 do (try-impose-3x3 grid row col))))))

(defun sudoku (grid)
  (let ((frame (make-application-frame 'sudoku :grid grid)))
    (run-frame-top-level frame)))

(define-sudoku-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-sudoku-command (com-assign :name t)
    ((cell 'integer :prompt "cell")
     (value 'integer :prompt "value"))
  (multiple-value-bind (row col) (floor cell 9)
    (setf (aref (grid *application-frame*) row col)
	  (list value))))

(define-sudoku-command (com-propagate :name t) ()
  (propagate-all (grid *application-frame*)))

(define-sudoku-command (com-impose :name t) ()
  (try-impose-all (grid *application-frame*)))

(define-sudoku-command (com-reset :name t) ()
  (init-grid *application-frame*))

(define-sudoku-command (com-toggle-view :name t) ()
  (setf *exploded-view* (not *exploded-view*)))

(defparameter *g1* #2A((0 0 4 0 0 0 0 0 7)
		       (0 0 2 5 0 0 1 0 0)
		       (0 0 0 0 6 0 0 4 8)
		       (0 0 0 4 3 0 0 0 6)
		       (3 0 9 0 0 0 4 0 1)
		       (6 0 0 0 5 1 0 0 0)
		       (8 3 0 0 9 0 0 0 0)
		       (0 0 1 0 0 5 2 0 0)
		       (9 0 0 0 0 0 3 0 0)))

(defparameter *g2* #2A((0 0 2 0 8 7 0 0 0)
		       (0 7 0 0 0 3 1 0 0)
		       (9 0 0 0 0 2 0 0 4)
		       (0 8 0 0 0 5 0 0 0)
		       (0 9 3 0 0 0 4 2 0)
		       (0 0 0 3 0 0 0 6 0)
		       (4 0 0 6 0 0 0 0 1)
		       (0 0 9 2 0 0 0 3 0)
		       (0 0 0 8 5 0 2 0 0)))

(defparameter *w1* #2A((9 0 0 0 0 0 6 0 0)
		       (0 0 3 0 7 0 0 0 4)
		       (0 0 0 0 5 8 7 0 2)
		       (2 5 0 0 0 0 0 0 1)
		       (0 0 0 2 0 3 0 0 0)
		       (3 0 0 0 0 0 0 7 6)
		       (7 0 8 4 6 0 0 0 0)
		       (4 0 0 0 8 0 9 0 0)
		       (0 0 6 0 0 0 0 0 7)))
