(in-package #:sudoku-gui)

(define-presentation-type sudoku-symbol ())
(define-presentation-type cell ())

(define-application-frame sudoku ()
  ((%game :initarg :game :accessor game)
   (%solution :initform nil :accessor solution)
   (%show-errors :initform nil :accessor show-errors)
   (%selected-cell :initform nil :accessor selected-cell)
   (%reason-field :initform nil :accessor reason-field)
   (%highlighted :initform nil :accessor highlighted)
   (%show-p-v :initform nil :accessor show-p-v-field))
  (:panes
   (quit-button
    :push-button
    :label " Quit "
    :activate-callback (lambda (button)
			 (declare (ignore button))
			 (frame-exit *application-frame*)))
   (show-errors-button
    :push-button
    :label (if (show-errors *application-frame*)
	       " Do not show errors "
	       "    Show errors     ")
    :activate-callback (lambda (button)
			 (setf (show-errors *application-frame*)
			       (not (show-errors *application-frame*)))
			 (setf (gadget-label button)
			       (if (show-errors *application-frame*)
				   " Do not show errors "
				   "    Show errors     "))
			 (redisplay-frame-panes *application-frame*)))
   (app :application
	:width 500 :height 550
	:display-function 'display-sudoku-pane)
   (reason-field
     :text-field
     :value "-")
   (show-possible-values
     (with-radio-box (:orientation :horizontal)
       (clim:radio-box-current-selection "Show") "No"))
   (step-button
     :push-button
     :label " Step "
     :max-width 40
     :activate-callback (lambda (button)
                          (declare (ignore button))
                          (let* ((game (game *application-frame*))
                                 (result (sudoku-solver:solve-one-step game))
                                 (ht (make-hash-table :test #'eql)))
                            (if result
                                (destructuring-bind (reason . cells) result
                                  (loop for hl-level in (list +blue+ +orange+)
                                        for cell-list in (if (listp (first cells))
                                                             cells
                                                             (list cells))
                                        do (dolist (c cell-list)
                                             (alexandria:ensure-gethash c ht hl-level)))
                                  (print reason *trace-output*)))
                            (setf (gadget-value 
                                    (reason-field *application-frame*))
                                  (or (first result)
                                      "No further inference"))
                            (setf (highlighted *application-frame*)
                                  ht)
                           (redisplay-frame-panes *application-frame*)))))
  (:layouts
   (default (vertically ()
	      (horizontally ()
		quit-button show-errors-button)
	      app
              (horizontally ()
                (setf (show-p-v-field *application-frame*)
		      show-possible-values)
                step-button
                (setf (reason-field *application-frame*)
                      reason-field))))))

(defmethod initialize-instance :after ((frame sudoku) &rest args &key)
  (declare (ignore args))
  (setf (solution frame)
	(sudoku-solver:stupid-solver (game frame))))

(defmethod (setf game) :after (new-game (frame sudoku))
  (setf (solution frame)
	(sudoku-solver:stupid-solver (game frame))))

(defun display-sudoku-pane (frame pane)
  (let* ((game (game frame))
	 (size (sudoku-game:size game))
	 (alphabet (sudoku-game:alphabet game))
	 (blank (sudoku-game:blank game))
	 (board (sudoku-game:board game)))
    (with-text-family (pane :sans-serif)
      (format pane "  ~a~%~%" (if (show-errors frame)
				  "Errors are shown in red"
				  "Errors are not shown"))
      (with-text-size (pane :huge)
	(with-text-face (pane :bold)
	  (loop for i in alphabet
		do (with-output-as-presentation (pane i 'sudoku-symbol)
		     (format pane "  ~a  " i)))
	  (with-translation (pane 20 80)
	    (sudoku-draw-board:draw-board
	     pane
	     size
	     (lambda (r c)
	       (let ((ink (if (equal (list r c) (selected-cell frame))
			      +yellow+
			      +white+)))
		 (with-output-as-presentation (pane (list r c) 'cell)
		     (draw-rectangle* pane 0 0 40 40 :ink ink)
                     (if (highlighted *application-frame*)
                         (let ((highlighted? (gethash (+ c (* r size size))
                                                      (highlighted *application-frame*))))
                           (if highlighted?
                               (draw-rectangle* pane 1 1 39 39 
                                                :line-thickness 2
                                                :ink highlighted?
                                                :filled nil))))))
	       (if (eql (aref board r c) blank)
		 (let ((allowed (sudoku-solver:allowed-values-in-cell game r c)))
		   (draw-text* pane (format nil "~{~a~}" allowed)
			       20 20
			       :text-size :small
			       :align-x :center :align-y :center
			       :ink +grey+))
		 (let ((ink (if (and (show-errors frame)
				     (not (eql (aref board r c)
					       (aref (solution frame) r c))))
				+red+
				+black+)))
		   (draw-text* pane (format nil "~a" (aref board r c))
			       20 20
			       :align-x :center :align-y :center
			       :ink ink))))
	     40 2 4)))))))
	
(defun sudoku ()
  (let* ((games sudoku-example-games:*games*)
	 (n (random (length games)))
	 (matrix (aref games n))
	 (game (sudoku-game:make-classic-game matrix))
	 (frame (make-application-frame 'sudoku :game game)))
    (run-frame-top-level frame)))

(define-sudoku-command (com-quit :keystroke (#\q :control)) ()
  (frame-exit *application-frame*))

(define-sudoku-command com-select-cell ((cell 'cell))
  (setf (selected-cell *application-frame*) cell)
  (unless (eql (apply #'aref (sudoku-game:board (game *application-frame*)) cell)
	       (sudoku-game:blank (game *application-frame*)))
    (apply #'(setf aref)
	   (sudoku-game:blank (game *application-frame*))
	   (sudoku-game:board (game *application-frame*))
	   cell)))

(define-presentation-to-command-translator select-cell
    (cell com-select-cell sudoku)
    (object)
  `(,object))    

(define-sudoku-command com-assign-selected-cell ((symbol 'sudoku-symbol))
  (let ((cell (selected-cell *application-frame*)))
    (unless (null cell)
      (apply #'(setf aref)
	     symbol
	     (sudoku-game:board (game *application-frame*))
	     cell))))

(define-presentation-to-command-translator assign-selected-cell
    (sudoku-symbol com-assign-selected-cell sudoku)
    (object)
  `(,object))