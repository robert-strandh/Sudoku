(in-package :sudoku.gui)

(defclass sudoku-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
      :display-function 'display-info
      :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (pane-name (master-pane pane))))

(defclass sudoku-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20))

(defclass sudoku-pane (esa-pane-mixin application-pane)
  ((contents :initform "hello" :accessor contents)))

(define-application-frame sudoku (esa-frame-mixin
				  standard-application-frame)
  ((%board :initarg :board :accessor board))
  (:panes
   (window (let* ((my-pane 
		   (make-pane 'sudoku-pane
			      :display-function 'display-my-pane
			      :command-table 'global-sudoku-table))
		  (my-info-pane
		   (make-pane 'sudoku-info-pane
			      :master-pane my-pane
			      :width 900)))
	     (setf (windows *application-frame*) (list my-pane))
	     (vertically (:width 500 :height 550)
			 (scrolling (:width 500 :height 500)
				    my-pane)
			 my-info-pane)))
   (minibuffer (make-pane 'sudoku-minibuffer-pane :width 500)))
  (:layouts
   (default
       (vertically (:scroll-bars nil :min-height 500)
		   window
		   minibuffer)))
  (:top-level (esa-top-level)))

(defparameter *cell-size* 50) ; including borders
(defparameter *thin-line-thickness* 1)
(defparameter *thick-line-thickness* 3)
(defparameter *offset* 21/2) ; where the upper-left corner is
(defparameter *alphabets*
  #(" 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ@"
    " ABCDEFGHIJKLMNOPQRSTUVWXY"))
(defparameter *current-alphabet* 0)

(defun draw-horizontal-line (pane grid-size x y thickness)
  (let ((grid-side (* grid-size *cell-size*)))
    (draw-rectangle* pane
		     (- x (/ *thick-line-thickness* 2))
		     (- y (/ thickness 2))
		     (+ x grid-side (/ *thick-line-thickness* 2))
		     (+ y (/ thickness 2)))))

(defun draw-vertical-line (pane grid-size x y thickness)
  (let ((grid-side (* grid-size *cell-size*)))
    (draw-rectangle* pane
		     (- x (/ thickness 2))
		     (- y (/ *thick-line-thickness* 2))
		     (+ x (/ thickness 2))
		     (+ y grid-side (/ *thick-line-thickness* 2)))))

(defun center-of-cell (x y)
  (values (+ *offset* (* (+ 1/2 x) *cell-size*))
	  (+ *offset* (* (+ 1/2 y) *cell-size*))))

(defun center-of-subcell (grid-size x1 y1 x2 y2)
  (let ((ulx (+ *offset*
		(* x1 *cell-size*)
		(/ *thick-line-thickness* 2)))
	(uly (+ *offset*
		(* y1 *cell-size*)
		(/ *thick-line-thickness* 2)))
	(extra (/ (- *cell-size* *thick-line-thickness*)
		  (1+ (round (sqrt grid-size))))))
    (values (+ ulx (* extra (1+ x2)))
	    (+ uly (* extra (1+ y2))))))

(defun sch (i)
  (aref (aref *alphabets* *current-alphabet*) i))

(defun draw-subcell (pane grid-size symbol x1 y1 x2 y2)
  (multiple-value-bind (xc yc)
      (center-of-subcell grid-size x1 y1 x2 y2)
    (with-text-size (pane :small)
      (draw-text* pane
		  (format nil "~a" (sch symbol))
		  xc yc
		  :align-x :center
		  :align-y :center))))

(defun draw-cell (pane grid-size cell x y)
  (let ((possibilities (subilo:possibilities cell)))
    (if (null (cdr possibilities))
	(multiple-value-bind (xc yc)
	    (center-of-cell x y)
	  (with-text-size (pane :huge)
	    (with-text-face (pane :bold)
	      (draw-text* pane (format nil "~a" (sch (car possibilities)))
			  xc yc
			  :align-x :center
			  :align-y :center))))
	(loop with root = (round (sqrt grid-size))
	      for yy from 0 below root
	      do (loop for xx from 0 below root
		       do (let ((symbol (1+ (+ (* yy root) xx))))
			    (when (member symbol possibilities)
			      (draw-subcell pane grid-size symbol x y xx yy))))))))

(defun display-my-pane (frame pane)
  (let* ((grid-size (subilo:grid-size (board frame)))
	 (root (round (sqrt grid-size))))
    (loop repeat (1+ grid-size)
	  for x from *offset* by *cell-size*
	  do (draw-vertical-line pane grid-size x *offset* *thin-line-thickness*))
    (loop repeat (1+ grid-size)
	  for y from *offset* by *cell-size*
	  do (draw-horizontal-line pane grid-size *offset* y *thin-line-thickness*))
    (loop repeat (1+ root)
	  for x from *offset* by (* root *cell-size*)
	  do (draw-vertical-line pane grid-size x *offset* *thick-line-thickness*))
    (loop repeat (1+ root)
	  for y from *offset* by (* root *cell-size*)
	  do (draw-horizontal-line pane grid-size *offset* y *thick-line-thickness*))
    (loop for x from 0 below grid-size
	  do (loop for y from 0 below grid-size
		   do (draw-cell pane grid-size (subilo:cell (board frame) x y) x y)))))

(defun random-element (list)
  (elt list (random (length list))))

(defun sudoku (&optional array &key (width 900) (height 400))
  "Starts up the sudoku application"
  (when (null array)
    (setf array (random-element (aref *examples* *current-size*))))
  (let ((frame (make-application-frame
		'sudoku
		:board (subilo:make-sudoku array)
		:width width :height height)))
    (run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands and key bindings

(define-command-table global-sudoku-table
    :inherit-from (global-esa-table keyboard-macro-table))


(define-command (com-toggle-alphabet
		 :name t
		 :command-table global-sudoku-table)
    ()
  (setf *current-alphabet*
	(- 1 *current-alphabet*)))

(set-key 'com-toggle-alphabet 'global-sudoku-table '((#\a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Examples

(defparameter *examples* (make-array 6 :initial-element '()))

(defparameter *current-size* 3)

(defun define-sudoku (array)
  (assert (= (array-dimension array 0)
	     (array-dimension array 1)))
  (assert (member (array-dimension array 0) '(4 9 16 25 36)))
  (push array (aref *examples* (round (sqrt (array-dimension array 0))))))


;;; hard (I think)
(define-sudoku #2A((0 0 4 0 0 0 0 0 7)
		   (0 0 2 5 0 0 1 0 0)
		   (0 0 0 0 6 0 0 4 8)
		   (0 0 0 4 3 0 0 0 6)
		   (3 0 9 0 0 0 4 0 1)
		   (6 0 0 0 5 1 0 0 0)
		   (8 3 0 0 9 0 0 0 0)
		   (0 0 1 0 0 5 2 0 0)
		   (9 0 0 0 0 0 3 0 0)))

;;; medium (I think)
(define-sudoku #2A((0 0 2 0 8 7 0 0 0)
		   (0 7 0 0 0 3 1 0 0)
		   (9 0 0 0 0 2 0 0 4)
		   (0 8 0 0 0 5 0 0 0)
		   (0 9 3 0 0 0 4 2 0)
		   (0 0 0 3 0 0 0 6 0)
		   (4 0 0 6 0 0 0 0 1)
		   (0 0 9 2 0 0 0 3 0)
		   (0 0 0 8 5 0 2 0 0)))

;;; medium
(define-sudoku #2A((8 0 0 3 0 0 0 5 0) 
		   (0 9 0 5 1 0 0 0 0)
		   (5 0 0 0 0 0 9 0 0)
		   (2 0 0 7 0 0 4 8 0)
		   (0 0 0 0 0 0 0 0 0)
		   (0 7 4 0 0 6 0 0 1)
		   (0 0 2 0 0 0 0 0 4)
		   (0 0 0 0 2 1 0 3 0)
		   (0 4 0 0 0 9 0 0 8)))

;;; hard
(define-sudoku #2A((0 7 0 0 2 0 1 0 8)
		   (8 0 1 0 0 0 0 2 0)
		   (0 0 0 3 0 0 0 0 0)
		   (0 4 3 0 0 0 8 0 0)
		   (0 0 0 6 0 5 0 0 0)
		   (0 0 8 0 0 0 2 5 0)
		   (0 0 0 0 0 9 0 0 0)
		   (0 6 0 0 0 0 7 0 4)
		   (2 0 5 0 7 0 0 9 0)))

;;; hard
(define-sudoku #2A((3 2 0 5 0 0 0 9 0)
		   (0 0 9 0 0 0 8 0 0)
		   (1 0 0 9 7 0 0 0 3)
		   (0 4 0 0 3 7 0 0 0)
		   (0 0 0 0 1 0 0 0 0)
		   (0 0 0 2 6 0 0 4 0)
		   (2 0 0 0 5 8 0 0 1)
		   (0 0 6 0 0 0 3 0 0)
		   (0 1 0 0 0 3 0 8 4)))
  