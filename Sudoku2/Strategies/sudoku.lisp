(defpackage :sudoku
  (:shadow :apply)
  (:export)
  (:use :common-lisp))

(in-package :sudoku)

(defun iota (n &optional (start 0) (step 1))
  (loop repeat n
	for i from start by step
	collect i))

(defclass cell ()
  ((%possibilites :initform (iota 9 1)
		  :initarg :possibilities
		  :accessor possibilities)))

(defclass modified-mixin ()
  ((%modified :initform t :accessor modified-p)))

(defclass area (modified-mixin)
  ((%cells :initarg :cells :reader cells)))

(defclass sudoku (modified-mixin)
  ((%areas :initform '() :reader areas)
   (%grid :initarg :grid :reader grid)))

(defmethod initialize-instance :after ((sudoku sudoku) &rest args)
  (declare (ignore args))
  (with-slots (%areas %grid) sudoku
    (loop for r from 0 below 9
	  do (push (make-instance 'area
		     :cells (loop for c from 0 below 9
				  collect (aref %grid r c)))
		   %areas))
    (loop for c from 0 below 9
	  do (push (make-instance 'area
		     :cells (loop for r from 0 below 9
				  collect (aref %grid r c)))
		   %areas))
    (loop for r from 0 below 9 by 3
	  do (loop for c from 0 below 9 by 3
		   do (push (make-instance 'area
			      :cells  (loop for dr from 0 below 3
					    append (loop for dc from 0 below 3
							 collect (aref %grid
								       (+ r dr)
								       (+ c dc)))))
			    %areas)))))

(defun make-sudoku (array)
  "make a sudoku from a 9x9 array containing natural numbers
   between 0 and 9, where 0 means unknown, and any other number
   means that the position must contain that number"
  (let ((grid (make-array '(9 9))))
    (loop for r from 0 below 9
	  do (loop for c from 0 below 9
		   do (setf (aref grid r c)
			    (make-instance 'cell
			      :possibilities (if (zerop (aref array r c))
						 (iota 9 1)
						 (list (aref grid r c)))))))
    (make-instance 'sudoku
		   :grid grid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strategies 

(defgeneric applicable-p (strategy area))
(defgeneric apply (sudoku strategy area))

(defmethod apply :after (sudoku strategy area)
  (setf (modified-p area) t
	(modified-p sudoku) t))

(defclass strategy () ())

(defparameter *strategies* '())

(defmacro define-strategy (name &optional slots)
  `(progn 
    (defclass ,name (strategy) ,slots)
    (push (make-instance ',name) *strategies*)))

;;; Strategy 1

(define-strategy number-exists-in-only-one-cell) 

(defmethod applicable-p ((strategy number-exists-in-only-one-cell) area)
  (loop for i from 1 to 9
	do (let ((cells-with-i (remove-if-not (lambda (cell)
						(member i (possibilities cell)))
					      cells)))
	     (when (and (= (length cells-with-i) 1)
			(> (length (possibilities (car cells-with-i))) 1))
	       (return-from applicable-p (values area (car cells-with-i) i))))))

;;; assume strategy is applicable
(defmethod apply (sudoku (strategy number-exists-in-only-one-cell) area)
  (multiple-value-bind (area cell number) (applicable-p strategy area)
    (setf (possibilities cell) (list number))))

;;; Strategy 2

(define-strategy cell-has-single-number)

(defmethod applicable-p ((strategy cell-has-single-number) area)
  (loop for cell in cells
	do (when (= (length (possibilities cell)) 1)
	     (let ((number (car (possibilities cell))))
	       (when (= (length (remove-if-not (lambda (cell)
						 (member number
							 (possibilities cell)))
					       cells))
			1)
		 (return-from applicable-p area cell number))))))

;; (defmethod apply (sudoku (strategy cell-has-single-number) area)
  
		       
(defun subsets (set size)
  "return a list of all the subsets of size SIZE of the set SET"
  (assert (<= 0 size (length set)))
  (cond ((zerop size) (list '()))
	((= size (length set)) (list set))
	(t (append (loop for subset in (subsets (cdr set) (1- size))
			 collect (cons (car set) subset))
		   (subsets (cdr set) size)))))
