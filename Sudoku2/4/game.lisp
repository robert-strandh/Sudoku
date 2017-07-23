(in-package #:sudoku-game)

(defgeneric size (game))
(defgeneric alphabet (game))
(defgeneric blank (game))
(defgeneric board (game))

(defclass game ()
  ((%size :initarg :size :reader size)
   (%alphabet :initarg :alphabet :reader alphabet)
   (%blank :initarg :blank :reader blank)
   (%board :initarg :board :reader board)
   ;; bit-arrays of allowed values for each cell
   (%allowed :initform nil :accessor allowed)
   ;; areas in which values need to be unique - typically rows, columns, and the 3x3 quadrants.
   (%rows :initform nil :accessor row-set)
   (%cols :initform nil :accessor col-set)
   (%areas :initform nil :accessor area-set)
   ;; all these areas in one list.
   (%all :initform nil :accessor all-sets)))

;;; take a 9x9 matrix containing numbers 0-9 
;;; (where 0 means a blank cell) and produce 
;;; a game instance
(defun make-classic-game (matrix)
  (let ((alphabet '(1 2 3 4 5 6 7 8 9))
	(board (make-array '(9 9))))
    ;; just copy the matrix
    (loop for r from 0 below 9
	  do (loop for c from 0 below 9
		   do (setf (aref board r c)
			    (aref matrix r c))))
    (make-instance 'game
      :size 3
      :alphabet alphabet
      :blank 0
      :board board)))

(defun board-length (game)
  (expt (size game) 2))
