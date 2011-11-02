(defpackage :sudoku.system
  (:use :common-lisp :asdf))

(in-package :sudoku.system)

(defsystem :sudoku
  :depends-on (:mcclim)
  :components ((:file "packages")
	       (:file "businesslogic" :depends-on ("packages"))
	       (:file "strategy" :depends-on ("packages"))
               (:file "gui" :depends-on ("packages" "strategy"))))

