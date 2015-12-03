(cl:in-package #:asdf-user)

(defsystem :sudoku
  :depends-on (:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "game")
   (:file "solver")
   (:file "draw-board")
   (:file "example-games")
   (:file "gui")))
