(defpackage #:sudoku-game
    (:use #:cl)
  (:export #:game
	   #:make-classic-game
	   #:size
	   #:alphabet
	   #:blank
	   #:board))

(defpackage #:sudoku-solver
    (:use #:cl)
  (:export #:stupid-solver))

(defpackage #:sudoku-draw-board
    (:use :cl)
  (:export #:draw-board))

(defpackage #:sudoku-gui
    (:use #:clim-lisp #:clim)
  (:export))

(defpackage #:sudoku-example-games
    (:use #:cl)
  (:export #:*games*))
