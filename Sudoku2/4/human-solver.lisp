(in-package #:sudoku-solver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A solver that works the way a human does.  

(defclass cell ()
  ((%possibilities :initform '() :initarg :possibilities :accessor possibilities)
   (%small-groups :initarg :small-groups :accessor small-groups)))

(defclass small-group ()
  ((%cells :initform '() :initarg :cells :accessor cells)
   (%large-groups :initform '() :initarg :large-groups :accessor large-groups)))

(defclass large-group ()
  ((%small-groups :initform '() :initarg :small-groups :accessor small-groups)
   (%solver :initform nil :initarg :solver :accessor solver)))

(defclass solver ()
  ((%large-groups :initform '() :initarg :large-groups :reader large-groups)
   (%board :initarg :board :reader board)))
  
(defun make-human-solver (game)
  (let* ((size (sudoku-game:size game))
	 (alphabet (sudoku-game:alphabet game))
	 (blank (sudoku-game:blank game))
	 (game-board (sudoku-game:board game))
	 (board (make-array (list (* size size) (* size size))))
	 (solver (make-instance 'solver :board board)))
    (loop for r from 0 below (* size size)
	  do (loop for c from 0 below (* size size)
		   do (let* ((contents (aref game-board r c))
			     (cell (make-instance 'cell
				     :possibilities (if (eql contents blank)
							(copy-list alphabet)
							(list contents)))))
			(setf (aref board r c) cell))))
    ;; do horizontal groups
    (let ((small-groups (make-array (list (* size size) size))))
      ;; create all the horizontal small groups
      (loop for r from 0 below (* size size)
	    do (loop for c from 0 below size
		     do (let ((small-group (make-instance 'small-group)))
			  (loop for cc from 0 below size
				do (push (aref board r (+ (* c 3) cc))
					 (cells small-group))
				do (push small-group
					 (small-groups (aref board r (+ (* c 3) cc)))))
			  (setf (aref small-groups r c) small-group))))
      ;; create all the horizontal large groups
      (loop for r from 0 below (* size size)
	    do (let ((large-group (make-instance 'large-group :solver solver)))
		 (push large-group (large-groups solver))
		 (loop for c from 0 below size
		       do (push (aref small-groups r c)
				(small-groups large-group))
		       do (push large-group
				(large-groups (aref small-groups r c))))))
      (loop for c from 0 below size
	    do (loop for r from 0 below size
		     do (let ((large-group (make-instance 'large-group :solver solver)))
			  (push large-group (large-groups solver))
			  (loop for rr from 0 below size
				do (push (aref small-groups (+ (* r 3) rr) c)
					 (small-groups large-group))
				do (push large-group
					 (large-groups (aref small-groups (+ (* r 3) rr) c))))))))
    ;; do vertical groups
    (let ((small-groups (make-array (list size (* size size)))))
      ;; create all the vertical small groups
      (loop for c from 0 below (* size size)
	    do (loop for r from 0 below size
		     do (let ((small-group (make-instance 'small-group)))
			  (loop for rr from 0 below size
				do (push (aref board (+ (* r 3) rr) c)
					 (cells small-group))
				do (push small-group
					 (small-groups (aref board (+ (* r 3) rr) c))))
			  (setf (aref small-groups r c) small-group))))
      ;; create all the vertical large groups
      (loop for c from 0 below (* size size)
	    do (let ((large-group (make-instance 'large-group :solver solver)))
		 (push large-group (large-groups solver))
		 (loop for r from 0 below size
		       do (push (aref small-groups r c)
				(small-groups large-group))
		       do (push large-group
				(large-groups (aref small-groups r c))))))
      (loop for r from 0 below size
	    do (loop for c from 0 below size
		     do (let ((large-group (make-instance 'large-group :solver solver)))
			  (push large-group (large-groups solver))
			  (loop for cc from 0 below size
				do (push (aref small-groups r (+ (* c 3) cc))
					 (small-groups large-group))
				do (push large-group
					 (large-groups (aref small-groups r (+ (* c 3) cc)))))))))
    solver))

;;; Very simple strategy: when a cell C contains a single possibility,
;;; that possibility can be eliminated from all other cells of all
;;; groups to which the cell belongs.

(defun strategy1 (solver)
  (flet ((propagate (cell)
	   (let ((applicable-p nil))
	     (when (null (cdr (possibilities cell)))
	       (loop for small-group in (small-groups cell)
		     do (loop for large-group in (large-groups small-group)
			      do (loop for small-group in (small-groups large-group)
				       do (loop for c in (cells small-group)
						do (unless (eq c cell)
						     (when (member (car (possibilities cell))
								   (possibilities c))
						       (setf applicable-p t)
						       (setf (possibilities c)
							     (remove (car (possibilities cell))
								     (possibilities c)))))))))))))
    (let* ((board (board solver))
	   (applicable-p nil))
      (loop for r from 0 below (array-dimension board 0)
	    do (loop for c from 0 below (array-dimension board 1)
		     do (when (propagate (aref board r c))
			  (setf applicable-p t))))
      applicable-p)))
