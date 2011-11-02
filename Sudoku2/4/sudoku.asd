(asdf:defsystem :sudoku
  :depends-on (:mcclim)
  :components
  ((:file "packages" :depends-on ())
   (:file "game" :depends-on ("packages"))
   (:file "solver" :depends-on ("packages" "game"))
   (:file "draw-board" :depends-on ("packages"))
   (:file "example-games" :depends-on ("packages"))
   (:file "gui" :depends-on ("packages" "game" "solver" "draw-board" "example-games"))
   ))
