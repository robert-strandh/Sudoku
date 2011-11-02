(defpackage :sudoku
  (:use :clim :clim-lisp)
  (:shadow :apply)
  (:export #:sudoku))

(in-package :sudoku)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main data structures

(defun iota (n &optional (start 0) (step 1))
  (loop repeat n
	for i from start by step
	collect i))

(defclass cell ()
  ((%possibilites :initform (iota 9 1)
		  :initarg :possibilities
		  :accessor possibilities)))

(defclass zone ()
  ((%cells :initarg :cells :reader cells)))

(defclass sudoku ()
  ((%initial :initarg :initial :reader initial)
   (%zones :initform '() :reader zones)
   (%grid :initform (make-array '(9 9)) :reader grid)))

(defun init-sudoku (sudoku)
  (with-slots (%initial %grid %zones) sudoku
    (loop for r from 0 below 9
	  do (loop for c from 0 below 9
		   do (setf (aref %grid r c)
			    (make-instance 'cell
			      :possibilities (if (zerop (aref %initial r c))
						 (iota 9 1)
						 (list (aref %initial r c)))))))
    (loop for r from 0 below 9
	  do (push (make-instance 'zone
		     :cells (loop for c from 0 below 9
				  collect (aref %grid r c)))
		   %zones))
    (loop for c from 0 below 9
	  do (push (make-instance 'zone
		     :cells (loop for r from 0 below 9
				  collect (aref %grid r c)))
		   %zones))
    (loop for r from 0 below 9 by 3
	  do (loop for c from 0 below 9 by 3
		   do (push (make-instance 'zone
			      :cells  (loop for dr from 0 below 3
					    append (loop for dc from 0 below 3
							 collect (aref %grid
								       (+ r dr)
								       (+ c dc)))))
			    %zones)))))

(defmethod initialize-instance :after ((sudoku sudoku) &rest args)
  (declare (ignore args))
  (init-sudoku sudoku))

(defun make-sudoku (array)
  "make a sudoku from a 9x9 array containing natural numbers
   between 0 and 9, where 0 means unknown, and any other number
   means that the position must contain that number"
  (make-instance 'sudoku :initial array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strategies 

(defgeneric applicable-p (strategy zone))
(defgeneric apply (sudoku strategy zone))

;;; (defmethod apply :after (sudoku strategy zone)
;;;  (setf (modified-p zone) t
;;; 	(modified-p sudoku) t))

(defclass strategy () ())

(defparameter *strategies* '())

(defmacro define-strategy (name &optional slots)
  `(progn 
    (defclass ,name (strategy) ,slots)
    (push (make-instance ',name) *strategies*)))

;;; Strategy 1

(define-strategy number-exists-in-only-one-cell) 

(defmethod applicable-p ((strategy number-exists-in-only-one-cell) zone)
  (loop for i from 1 to 9
	do (let ((cells-with-i (remove-if-not (lambda (cell)
						(member i (possibilities cell)))
					      (cells zone))))
	     (when (and (= (length cells-with-i) 1)
			(> (length (possibilities (car cells-with-i))) 1))
	       (return-from applicable-p (values zone (list (car cells-with-i)) i))))))

(defmethod apply (sudoku (strategy number-exists-in-only-one-cell) zone)
  (multiple-value-bind (zone cells number) (applicable-p strategy zone)
    (declare (ignore zone))
    (setf (possibilities (car cells)) (list number))))

;;; Strategy 2

(define-strategy cell-has-single-number)

(defmethod applicable-p ((strategy cell-has-single-number) zone)
  (loop for cell in (cells zone)
	do (let ((possibilities (possibilities cell)))
	     (when (= (length possibilities) 1)
	       (let ((number (car possibilities)))
		 (when (> (length (remove-if-not (lambda (cell)
						   (member number (possibilities cell)))
						 (cells zone)))
			  1)
		   (return-from applicable-p (values zone (list cell) number))))))))

(defmethod apply (sudoku (strategy cell-has-single-number) zone)
  (multiple-value-bind (zone cells number) (applicable-p strategy zone)
    (loop for cell in (cells zone)
	  do (unless (eq cell (car cells))
	       (setf (possibilities cell)
		     (remove number (possibilities cell)))))))
		       
(defun subsets (set size)
  "return a list of all the subsets of size SIZE of the set SET"
  (assert (<= 0 size (length set)))
  (cond ((zerop size) (list '()))
	((= size (length set)) (list set))
	(t (append (loop for subset in (subsets (cdr set) (1- size))
			 collect (cons (car set) subset))
		   (subsets (cdr set) size)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GUI

(defclass cell-pane (application-pane)
  ((%row :initarg :row :reader row)
   (%col :initarg :col :reader col)))

(defun make-3x3-square (r c)
  (climi::bordering (:border-width 3)
    (make-pane 'grid-pane
      :width 165 :height 165 :min-width 165 :min-height 165
      :contents (loop for dr from 0 below 3
		      collect (loop for dc from 0 below 3
				    collect (climi::bordering ()
					      (make-pane 'cell-pane
					        :row (+ r dr) :col (+ c dc)
						:width 50 :height 50
						:display-function 'display-cell)))))))

(define-application-frame sudoku-frame ()
  ((model :initarg :model :reader model)
   (selected-strategy :initform nil :accessor selected-strategy)
   (selected-cells :initform '() :accessor selected-cells)
   (selected-zone :initform nil :accessor selected-zone)
   (selected-number :initform 0 :accessor selected-number))
  (:panes (app (make-pane 'grid-pane
		 :width 500 :height 500 :min-width 500 :min-height 500
		 :contents (loop for r from 0 below 9 by 3
				 collect (loop for c from 0 below 9 by 3
					       collect (make-3x3-square r c)))))
	  (int :interactor :width 500 :height 200 :max-height 200))
  (:layouts (default (vertically () app int))))

(defparameter *total-view* nil)

(defgeneric explain (strategy frame))

(defmethod explain ((strategy number-exists-in-only-one-cell) frame)
  (format (frame-standard-input frame)
	  "In the zone shown in yellow, the number 
~a exists only in the cell shown in red"
	  (selected-number frame)))

(defmethod explain ((strategy cell-has-single-number) frame)
  (format (frame-standard-input frame)
	  "In the cell shown in red, ~s is the only
number, but that number is possible in
other cells in the zone shown in yellow. 
It can therefore be eliminated as possible
from those other cells."
	  (selected-number frame)))

(defun display-cell (frame pane)
  (let* ((cell (aref (grid (model frame)) (row pane) (col pane)))
	 (possibilities (possibilities cell)))
    (unless (null (selected-strategy frame))
      (cond ((member cell (selected-cells frame))
	     (draw-rectangle* pane 0 0 50 50 :ink +red+))
	    ((member cell (cells (selected-zone frame)))
	     (draw-rectangle* pane 0 0 50 50 :ink +yellow+))
	    (t nil)))
    (if (and (not (null possibilities))
	     (null (cdr possibilities)))
	(with-text-size (pane :huge)
	  (with-text-face (pane :bold)
	    (draw-text* pane
			(format nil "~a" (if (null possibilities) "*" (car possibilities)))
			25 25
			:align-x :center :align-y :center)))
	(with-text-size (pane :small)
	  (with-drawing-options (pane :ink +blue+)
	    (loop for i from 1 to 9
		  do (princ (cond ((not *total-view*) #\space)
				  ((member i possibilities) i)
				  (t #\-))
			    pane)
		  do (princ #\space pane)
		  do (when (zerop (mod i 3))
		       (terpri pane))))))))

(defmethod redisplay-frame-panes :after ((frame sudoku-frame) &key force-p)
  (declare (ignore force-p))
  (if (null (selected-strategy frame))
      (format (frame-standard-input frame)
	      "No strategy applies~%")
      (progn (explain (selected-strategy frame) frame)
	     (terpri (frame-standard-input frame)))))

(defun sudoku (array)
  (let* ((model (make-sudoku array))
	 (frame (make-application-frame 'sudoku-frame :model model)))
    (run-frame-top-level frame)))

(define-sudoku-frame-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-sudoku-frame-command (com-reset :name t) ()
  (init-sudoku (model *application-frame*)))

(define-sudoku-frame-command (com-toggle-view :name t) ()
  (setf *total-view* (not *total-view*)))

(define-sudoku-frame-command (com-check-strategy :name t) ()
  (let ((frame *application-frame*))
    (loop for strategy in *strategies*
	  do (loop for zone in (zones (model *application-frame*))
		   do (multiple-value-bind (zone cells number)
			  (applicable-p strategy zone)
			(unless (null zone)
			  (setf (selected-strategy frame) strategy
				(selected-zone frame) zone
				(selected-cells frame) cells
				(selected-number frame) number)
			  (return-from com-check-strategy)))))))

(define-sudoku-frame-command (com-apply-strategy :name t) ()
  (let ((frame *application-frame*))
    (unless (null (selected-strategy frame))
      (apply (model frame) (selected-strategy frame) (selected-zone frame)))
    (setf (selected-strategy frame) nil)))

;;; hard (I think)
(defparameter *g1* #2A((0 0 4 0 0 0 0 0 7)
		       (0 0 2 5 0 0 1 0 0)
		       (0 0 0 0 6 0 0 4 8)
		       (0 0 0 4 3 0 0 0 6)
		       (3 0 9 0 0 0 4 0 1)
		       (6 0 0 0 5 1 0 0 0)
		       (8 3 0 0 9 0 0 0 0)
		       (0 0 1 0 0 5 2 0 0)
		       (9 0 0 0 0 0 3 0 0)))

;;; medium (I think)
(defparameter *g2* #2A((0 0 2 0 8 7 0 0 0)
		       (0 7 0 0 0 3 1 0 0)
		       (9 0 0 0 0 2 0 0 4)
		       (0 8 0 0 0 5 0 0 0)
		       (0 9 3 0 0 0 4 2 0)
		       (0 0 0 3 0 0 0 6 0)
		       (4 0 0 6 0 0 0 0 1)
		       (0 0 9 2 0 0 0 3 0)
		       (0 0 0 8 5 0 2 0 0)))

;;; medium
(defparameter *g3* #2A((8 0 0 3 0 0 0 5 0) 
		       (0 9 0 5 1 0 0 0 0)
		       (5 0 0 0 0 0 9 0 0)
		       (2 0 0 7 0 0 4 8 0)
		       (0 0 0 0 0 0 0 0 0)
		       (0 7 4 0 0 6 0 0 1)
		       (0 0 2 0 0 0 0 0 4)
		       (0 0 0 0 2 1 0 3 0)
		       (0 4 0 0 0 9 0 0 8)))

;;; hard
(defparameter *g4* #2A((0 7 0 0 2 0 1 0 8)
		       (8 0 1 0 0 0 0 2 0)
		       (0 0 0 3 0 0 0 0 0)
		       (0 4 3 0 0 0 8 0 0)
		       (0 0 0 6 0 5 0 0 0)
		       (0 0 8 0 0 0 2 5 0)
		       (0 0 0 0 0 9 0 0 0)
		       (0 6 0 0 0 0 7 0 4)
		       (2 0 5 0 7 0 0 9 0)))

;;; hard
(defparameter *g5* #2A((3 2 0 5 0 0 0 9 0)
		       (0 0 9 0 0 0 8 0 0)
		       (1 0 0 9 7 0 0 0 3)
		       (0 4 0 0 3 7 0 0 0)
		       (0 0 0 0 1 0 0 0 0)
		       (0 0 0 2 6 0 0 4 0)
		       (2 0 0 0 5 8 0 0 1)
		       (0 0 6 0 0 0 3 0 0)
		       (0 1 0 0 0 3 0 8 4)))
