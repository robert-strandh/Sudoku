(in-package :sudoku.strategy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Protocol 

(defgeneric applicable-p (strategy))
(defgeneric apply (strategy))
(defun all-strategies ())
(defgeneric tell (stream strategy language))
(defgeneric short-explain (stream strategy langauge))
(defgeneric long-explain (stream strategy language))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal generic functions 

(defgeneric first-part (strategy))
(defgeneric second-part (strategy))

;;; (defmethod apply :after (sudoku strategy zone)
;;;  (setf (modified-p zone) t
;;; 	(modified-p sudoku) t))

(defclass strategy ()
  ((%first-part :initarg :first-part :reader first-part)
   (%second-part :initarg :second-part :reader second-part)))
  
(defparameter *strategies* '())

(defmacro define-strategy (name &optional slots)
  `(progn 
    (defclass ,name (strategy) ,slots)
    (push ',name *strategies*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strategy 1.  

(define-strategy number-exists-in-only-one-cell) 

(defmethod short-explain (stream (strategy number-exists-in-only-one-cell) language)
  (format stream "Symbol unique in group"))

(defmethod long-explain (stream  (strategy number-exists-in-only-one-cell) language)
  (format stream "In a group of cells,
if a symbol exists in
only one of these cells
then that symbol is the
only one possible in 
that cell."))

(defmethod applicable-p ((strategy number-exists-in-only-one-cell))
  (let ((first-part (first-part strategy))
	(second-part (second-part strategy)))
    nil))

(defmethod apply ((strategy number-exists-in-only-one-cell))
  (multiple-value-bind (zone cells number) (applicable-p strategy)
    (declare (ignore zone))
    (setf (subilo:possibilities (car cells)) (list number))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Strategy 2.

(define-strategy cell-has-single-number)

(defmethod short-explain (stream (strategy cell-has-single-number) language)
  (format stream "Singleton cell symbol eliminates"))

(defmethod long-explain (stream (strategy cell-has-single-number) language)
  (format stream "When a cell in a group,
has a single possible symbol,
then that symbol can be 
eliminated from the possi-
bilities of other cells
in the same group."))

(defmethod applicable-p ((strategy cell-has-single-number))
  (let ((first-part (first-part strategy))
	(second-part (second-part strategy)))
    (and (= 1 (length second-part))
	 (= 1 (length (subilo:possibilities (car second-part))))
	 (let ((symbol (car (subilo:possibilities (car second-part)))))
	   (some (lambda (cell) (member symbol (subilo:possibilities cell)))
		 first-part)))))

(defmethod apply ((strategy cell-has-single-number))
  (assert (applicable-p strategy))
  (let* ((first-part (first-part strategy))
	 (second-part (second-part strategy))
	 (symbol (car (subilo:possibilities (car second-part)))))
    (mapc (lambda (cell)
	    (setf (subilo:possibilities cell) (remove symbol (subilo:possibilities cell))))
	  first-part)))
		       
