(defpackage #:sudoku-game
    (:use #:cl)
  (:export #:game
	   #:make-classic-game
	   #:size
	   #:alphabet
	   #:blank
	   #:board
	   #:allowed
	   #:row-set
	   #:col-set
	   #:area-set
	   #:all-sets
	   #:board-length))

(defpackage #:sudoku-solver
    (:use #:cl)
  (:export #:stupid-solver
           #:allowed-values-in-cell
           #:solve-one-step))

(defpackage #:sudoku-draw-board
    (:use :cl)
  (:export #:draw-board))

(defpackage #:sudoku-gui
    (:use #:clim-lisp #:clim)
  (:export))

(defpackage #:sudoku-example-games
    (:use #:cl)
  (:export #:*games*))
