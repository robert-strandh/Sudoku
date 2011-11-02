(in-package #:sudoku-solver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stupid solver.  For each empty square, try all symbols that
;;; respect the Sudoku constraints, and recurse for the next square.
;;; if a solution exists, one will always be found (but it may take a
;;; very long time).  Also, there is no guarantee that the solution
;;; found is unique.

(defun check-ok (matrix r c size)
  (loop for rr from 0 below (* size size)
	do (unless (= rr r)
	     (when (eql (aref matrix r c) (aref matrix rr c))
	       (return-from check-ok nil))))
  (loop for cc from 0 below (* size size)
	do (unless (= cc c)
	     (when (eql (aref matrix r c) (aref matrix r cc))
	       (return-from check-ok nil))))
  (loop with r0 = (* (floor r size) size)
	with c0 = (* (floor c size) size)
	for rr from r0
	repeat size
	do (loop for cc from c0
		 repeat size
		 do (unless (and (= rr r) (= cc c))
		      (when (eql (aref matrix r c) (aref matrix rr cc))
			(return-from check-ok nil)))))
  t)

(defun stupid-solver (game)
  (let* ((alphabet (sudoku-game:alphabet game))
	 (board (sudoku-game:board game))
	 (size (sudoku-game:size game))
	 (matrix (make-array (list (* size size) (* size size))))
	 (blank (sudoku-game:blank game)))
    (labels ((solver-aux (r c)
	       (cond ((= r (* size size))
		      (throw 'done nil))
		     ((not (eql (aref matrix r c) blank))
		      (if (= c (1- (* size size)))
			  (solver-aux (1+ r) 0)
			  (solver-aux r (1+ c))))
		     (t (loop for candidate in alphabet
			      do (setf (aref matrix r c) candidate)
			      do (when (check-ok matrix r c size)
				   (if (= c (1- (* size size)))
				       (solver-aux (1+ r) 0)
				       (solver-aux r (1+ c)))))
			(setf (aref matrix r c) blank)))))
      ;; initialize the matrix from the board
      (loop for r from 0 below (* size size)
	    do (loop for c from 0 below (* size size)
		     do (setf (aref matrix r c)
			      (aref board r c))))
      (catch 'done
	(solver-aux 0 0)
	;; if we come here, then there is no solution
	(return-from stupid-solver nil)))
    matrix))
			      
