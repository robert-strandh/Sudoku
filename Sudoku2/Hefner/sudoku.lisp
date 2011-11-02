(in-package :clim-user)

;;; "Sudoku Board"

;;; (C) Copyright 2006 by Andy Hefner (ahefner@gmail.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; -------------------------------------------------------------------

;;; This is a quick hack I threw together so that I could try playing
;;; the game "Sudoku", which automates application of the obvious
;;; elimination across columns/rows/whatevers, but does not attempt
;;; to solve the puzzle. The intended usage is to take a puzzle from
;;; the newpaper, clicking to enter the puzzle in as it's printed,
;;; and then select "Lock Board" from the menu when done. This locks
;;; the fixed squares so they can't be accidentally changed while
;;; attempting to solve the puzzle.

;;; This is not a particularly good hack. It attempts to cleverly
;;; use CLIM table formatting to lay out the board, but with the
;;; ridiculous twist of building it as a 3x3 table of 3x3 tables,
;;; so that each 3x3 block can be distinguished visually. This
;;; misbehaves once enough squares have been checked off that the
;;; table cells start to collapse. It's also slower than I'd prefer.
;;; It could be rewritten in a more traditional manner, figuring out
;;; sizes then calling draw-rectangle and draw-text. Various other
;;; McCLIM bugs can also be observed. It may not paint itself,
;;; initially, and has the bizarre tendency to walk across the screen
;;; upon each redisplay.

(defun new-sukodu-board () (make-array '(9 9) :initial-element nil))

(define-command-table sudoku-board-commands)

(define-application-frame sudoku ()
  ((board :initform (new-sukodu-board) :accessor board))
  (:command-table (sudoku
                   :inherit-from (sudoku-board-commands)
                   :menu (("Board" :menu sudoku-board-commands))))
  (:panes (main :application-pane
                :background +gray90+
                :display-function 'sudoku-display-function
                :display-time :command-loop))
  (:layouts
   (default main)))

(defstruct (sudoku-cell
             (:constructor make-sudoku-cell (value index-0 index-1)))
  value
  index-0
  index-1)

(define-presentation-type sudoku-possibility ())

(defclass locked-sudoku-cell ()
  ((value :accessor value :initarg :value)))

(defmethod value (x) x)

(defun compute-mask (board i j)
  (let ((mask (1- (expt 2 10))))
    (dotimes (k 9)
      (flet ((inner (x)
               (when x (setf mask (logand mask (lognot (expt 2 (1- x))))))))
        (inner (value (aref board i k)))
        (inner (value (aref board (+ (* 3 (floor i 3)) (floor k 3)) (+ (* 3 (floor j 3)) (mod k 3)))))
        (inner (value (aref board k j)))))
    mask))

(defgeneric display-sudoku-cell (cell board i j stream))

(defmethod display-sudoku-cell ((cell number) board i j stream)
  (with-drawing-options (stream :text-style (make-text-style :sans-serif :bold :huge)
                                :ink +red+)
    (with-output-as-presentation (stream (make-sudoku-cell nil i j) 'sudoku-possibility)
      (princ (value cell) stream))))

(defmethod display-sudoku-cell ((cell locked-sudoku-cell) board i j stream)
  (with-text-style (stream (make-text-style :sans-serif :bold :huge))    
    (princ (value cell) stream)))

(defmethod display-sudoku-cell ((cell null) board i j stream)
  (surrounding-output-with-border (stream :shape :rounded)
    (with-drawing-options (stream :text-style (make-text-style :fixed :roman :normal))
      (let ((mask (compute-mask board i j)))
        (formatting-table (stream)
          (dotimes (p 3)
            (formatting-row (stream)
              (dotimes (q 3)
                (formatting-cell (stream)
                  (if (zerop (logand (expt 2 (+ (* p 3) q)) mask))
                      (princ " " stream)
                      (with-output-as-presentation (stream (make-sudoku-cell (+ 1 (* p 3) q) i j) 'sudoku-possibility)
                        (princ (+ 1 (* p 3) q) stream))))))))))))

(defun display-sudoku-board (board stream)
  (formatting-table (stream)
    (dotimes (i 3)
      (formatting-row (stream)
        (dotimes (j 3)
          (formatting-cell (stream :align-x :center :align-y :center)
            (surrounding-output-with-border (stream :shape :rectangle :background +gray96+ :shadow +gray80+)
              (formatting-table (stream :x-spacing '(1 :character)
                                        :y-spacing '(1 :character))
                (dotimes (k 3)
                  (formatting-row (stream)
                    (dotimes (l 3)
                      (formatting-cell (stream :align-x :center :align-y :center
                                               :min-width  '(6 :character)
                                               :min-height '(6 :character))
                        (display-sudoku-cell (aref board (+ (* 3 i) k) (+ (* 3 j) l)) board (+ (* 3 i) k) (+ (* 3 j) l) stream)))))))))))))

(defun draw-table-foo (table-record)
  table-record)

(defun sudoku-display-function (frame stream)
  (draw-table-foo (display-sudoku-board (board frame) stream))
  (let* ((pane (find-pane-named frame 'main))
         (history (stream-output-history pane)))
    ;; The usual hack.. worse than usual, because we need to size the frame
    ;; to fit, and McCLIM (the CLX backend, anyway) goes a little crazy,
    ;; and somehow moves the window every time.
    ;; Setting the sizes of the  to :compute might be worth trying.
    (change-space-requirements pane                ; ...
                               :min-width  (bounding-rectangle-width  history)
                               :width      (bounding-rectangle-width  history)
                               :min-height (bounding-rectangle-height history)
                               :height     (bounding-rectangle-height history)
                               :resize-frame t)))

(define-sudoku-command (com-set-cell)
    ((row    '(integer 0 8))
     (column '(integer 0 8))
     (value  '(or (integer 1 9) null)))
  (setf (aref (board *application-frame*) row column) value))

(define-presentation-to-command-translator cell-unset-translator
    (sudoku-possibility com-set-cell sudoku
                        :gesture :select)
  (object)
  (list (sudoku-cell-index-0 object)
        (sudoku-cell-index-1 object)
        (sudoku-cell-value object)))

(define-command (com-lock-board :name "Lock Board"
                                :command-table sudoku-board-commands
                                :menu t)
    ()
  (let ((board (board *application-frame*)))
    (dotimes (i (array-dimension board 0))
      (dotimes (j (array-dimension board 1))
        (when (integerp (aref board i j))
          (setf (aref board i j) (make-instance 'locked-sudoku-cell :value (aref board i j))))))))


(define-command (com-reset-board :name "Reset Board"
                                 :command-table sudoku-board-commands
                                 :menu t)
    ()
  (setf (board *application-frame*) (new-sukodu-board)))
