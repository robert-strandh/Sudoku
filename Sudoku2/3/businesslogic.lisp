(in-package :sudoku.businesslogic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Protocol

(defgeneric cell (sudoku x y))
(defgeneric possibilities (cell))
(defgeneric (setf possibilities) (new-possibilities cell))
(defgeneric grid-size (sudoku))
;;; (defun make-sudoku (array)...)

(defun iota (n &optional (start 0) (step 1))
  (loop repeat n
	for i from start by step
	collect i))

(defclass cell ()
  ((%possibilites :initarg :possibilities :accessor possibilities)))

(defclass group ()
  ((%cells :initarg :cells :reader cells)))

(defclass sudoku ()
  ((%initial :initarg :initial :reader initial)
   (%groups :initform '() :reader groups)
   (%grid :reader grid)))

(defmethod grid-size ((sudoku sudoku))
  (array-dimension (grid sudoku) 0))

(defmethod cell (sudoku x y)
  (aref (grid sudoku) y x))

(defun init-sudoku (sudoku)
  (with-slots (%initial %grid %groups) sudoku
    (let* ((grid-size (array-dimension %initial 0))
	   (root (round (sqrt grid-size))))
      (setf %grid (make-array (list grid-size grid-size)))
      (loop for r from 0 below grid-size
	    do (loop for c from 0 below grid-size
		     do (setf (aref %grid r c)
			      (make-instance 'cell
				:possibilities (if (zerop (aref %initial r c))
						   (iota grid-size 1)
						   (list (aref %initial r c)))))))
      (loop for r from 0 below grid-size
	    do (push (make-instance 'group
		       :cells (loop for c from 0 below grid-size
				    collect (aref %grid r c)))
		     %groups))
      (loop for c from 0 below grid-size
	    do (push (make-instance 'group
		       :cells (loop for r from 0 below grid-size
				    collect (aref %grid r c)))
		     %groups))
      (loop for r from 0 below grid-size by root
	    do (loop for c from 0 below grid-size by root
		     do (push (make-instance 'group
				:cells  (loop for dr from 0 below root
					      append (loop for dc from 0 below root
							   collect (aref %grid
									 (+ r dr)
									 (+ c dc)))))
			      %groups))))))

(defmethod initialize-instance :after ((sudoku sudoku) &rest args)
  (declare (ignore args))
  (init-sudoku sudoku))

(defun make-sudoku (array)
  "make a sudoku from an nxn array with each position containing either 
   a natural number between 1 and n inclusive, indicating that the position
   has a single symbol possible, or 0, to indicate that 
   the position may contain any symbol betweeen 1 and n inclusive"
  (make-instance 'sudoku :initial array))

(defun partition (set)
  "Partition a set into two nonempty subsets in all possible ways."
  (assert (not (null set)))
  (labels ((aux (set)
	     (if (null (cdr set))
		 (list (cons (list (car set)) '())
		       (cons '() (list (car set))))
		 (let ((remaining (aux (cdr set))))
		   (append (mapcar (lambda (partition)
				     (cons (cons (car set) (car partition))
					   (cdr partition)))
				   remaining)
			   (mapcar (lambda (partition)
				     (cons (car partition)
					   (cons (car set) (cdr partition))))
				   remaining))))))
    (remove-if (lambda (partition)
		 (or (null (car partition))
		     (null (cdr partition))))
	       (aux set))))
