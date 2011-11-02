(defpackage :sudoku
    (:use)
  (:export #:sudoku))

(defpackage :sudoku.businesslogic
    (:nicknames :subilo)
  (:use :common-lisp)
  (:export #:cell            ; generic function (cell x y)
	   #:make-sudoku     ; function (&optional array)
	   #:grid-size       ; generic function (sudoku)
	   #:possibilities   ; generic function (cell), also (setf possibilities)
	   ))

(defpackage :sudoku.strategy
    (:nicknames :sustra)
  (:use :common-lisp)
  (:shadow #:apply)
  (:export #:all-strategies #:applicable-p #:apply
	   #:tell #:short-explain #:long-explain))

(defpackage :sudoku.gui
    (:use :clim-lisp :clim :esa :sudoku)
  (:export))
