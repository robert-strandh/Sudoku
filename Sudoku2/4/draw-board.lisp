(in-package #:sudoku-draw-board)

(defun draw-board (pane
		   size
		   draw-cell
		   cell-width
		   line-width
		   thick-line-width)
  (flet ((draw-small-square (r c)
	   (let ((w (+ (* size cell-width)
		       (* (1- size) line-width)))
		 (cell-dist (+ line-width cell-width)))
	     ;; draw the cells
	     (loop for rr from 0 below size
		   for yy from thick-line-width by cell-dist
		   do (clim:with-translation (pane 0 yy)
			(loop for cc from 0 below size
			      for xx from thick-line-width by cell-dist
			      do (clim:with-translation (pane xx 0)
				   (funcall draw-cell
					    (+ (* r size) rr)
					    (+ (* c size) cc))))))
	     ;; draw the lines
	     ;; thick lines first
	     (clim:draw-rectangle* pane
				   0
				   0
				   (+ w (* 2 thick-line-width))
				   thick-line-width)
	     (clim:draw-rectangle* pane
				   0
				   0
				   thick-line-width
				   (+ w (* 2 thick-line-width)))
	     ;; thin lines
	     (loop for rr from 1 below size
		   for yy from (+ thick-line-width cell-width) by cell-dist
		   do (clim:draw-rectangle*
		       pane
		       0 yy
		       (+ w thick-line-width) (+ yy line-width)))
	     (loop for cc from 1 below size
		   for xx from (+ thick-line-width cell-width) by cell-dist
		   do (clim:draw-rectangle*
		       pane
		       xx 0
		       (+ xx line-width) (+ w thick-line-width))))))
    (loop for r from 0 below size
	  for y from 0 by (+ (* size cell-width)
			     (* (1- size) line-width)
			     thick-line-width)
	  do (loop for c from 0 below size
		   for x from 0 by (+ (* size cell-width)
				      (* (1- size) line-width)
				      thick-line-width)
		   do (clim:with-translation (pane x y)
			(draw-small-square r c)))))
  (let ((w (+ (* size size cell-width)
	      (* (1- size) size line-width)
	      (* size thick-line-width))))
    (clim:draw-rectangle* pane
			  0 w
			  (+ w thick-line-width) (+ w thick-line-width))
    (clim:draw-rectangle* pane
			  w 0
			  (+ w thick-line-width) (+ w thick-line-width))))
