(in-package #:sudoku-solver)


;;; Solving a SUDOKU by using rules.
;; 
;; This assumes that the cell values are integers starting from 1;
;; if that assumption doesn't hold any more,
;; BITNUMBER-FROM-VALUE and VALUE-FROM-BITNUMBER need to be fixed.
;; But MAKE-CLASSIC-GAME has the same convention, so we won't bother here.



;;; Infrastructure.

(deftype allowed-in-cell (size) `(simple-array bit ,size))

(defun all-allowed-in-cell (size &optional (init 1)) 
  (make-array size
              :element-type 'bit
              :initial-element init))

(defun make-allowed-array (length)
  (loop with a = (make-array (list length length)
                             :element-type `(allowed-in-cell ,length))
        for i below (* length length)
        do (setf (row-major-aref a i)
                 (all-allowed-in-cell length))
        finally (return a)))

(defun mask-with-only-one-bit-at (bit length)
  (let ((a (all-allowed-in-cell length 0)))
    (setf (sbit a bit) 1)
    a))


(defun bitnumber-from-value (value)
  (1- value))
(defun value-from-bitnumber (bit)
  (1+ bit))


(defun next-set-bit (mask current)
  "Returns the next set bit _after_ CURRENT."
  (declare (type (allowed-in-cell *) mask)
           (type fixnum current))
  (loop with length = (array-dimension mask 0)
  for i from (1+ current) below length
        if (is-allowed mask i)
        return i))

(defun first-set-bit (mask)
  (next-set-bit mask -1))


(defun allowed-bits-in-mask (mask &optional (fn #'identity))
  (loop for bit = (first-set-bit mask)
        then (next-set-bit mask bit)
        while bit
        collect (funcall fn bit)))

(defun allowed-values-from-mask (mask)
  (allowed-bits-in-mask mask #'value-from-bitnumber))

(defun allowed-values-in-cell (game r c)
  (let ((allowed (sudoku-game:allowed game)))
    (if allowed
        (allowed-values-from-mask
          (aref (sudoku-game:allowed game) r c)))))

(defun clear-bit (allowed location bit)
  "Clears the BIT at LOCATION in ALLOWED; returns the previous value (0 or 1)."
  #+(or)
  (if (member location '(19 2600))
      (format *trace-output*
              "clear bit ~a in location ~a; was ~a, pos ~a~%"
              bit location (row-major-aref allowed location)
              (subseq 
                (second (sb-debug:list-backtrace :count 2))
                2 4)
              ))
  (shiftf (sbit (row-major-aref allowed location)
                bit)
        0))

(defun clear-bits-in-set (allowed bit cell-set)
  "Clears BIT in the locations of CELL-SET in ALLOWED.
   Returns the number of cleared bits (that were still set previously)."
  (loop for loc in cell-set
        sum (clear-bit allowed loc bit)))


(defun is-blank (game &key value location)
  (if location
      (setf value 
            (row-major-aref (sudoku-game:board game)
                            location)))
  (eql value
       (sudoku-game:blank game)))
  


(defun is-allowed (mask bit)
  (= 1 (sbit mask bit)))

(defun is-not-possible (mask bit)
  (not (is-allowed mask bit)))

(defun number-of-set-bits (mask)
  (reduce #'+ 
          mask))

(defun rule-outcome (involved-cells 
                      reason &rest reason-args)
  (let ((*print-right-margin* 200))
    (list* (apply #'format nil reason reason-args)
          involved-cells)))


(defun rule-got-value (game loc new-value 
                            involved-cells 
                            reason &rest reason-args)
  (let ((*print-right-margin* 200))
    ;; should we copy that before modifying?
    (setf (row-major-aref (sudoku-game:board game) loc)
          new-value)
    ;; Make the primary location more important (to highlight)
    (rule-outcome (list (list loc)
                         involved-cells)
                  "Cell #~d must be ~a, because ~?"
                  loc new-value
                  reason reason-args)))



;;; Cell sets.
;;
;; These use a single integer to specify a location,
;; compatible to ROW-MAJOR-AREF.

(defun col-locations (which length)
  "returns a list of locations for the given column."
  (loop for i below length
        collect (+ which (* length i))))

(defun row-locations (which length)
  "Returns a list of locations for the given row."
  (loop for i below length
        collect (+ i (* length which))))

(defun quad-locations (which size length)
  "Returns a list of locations for the given quadrant;
   WHICH must be an integer from 0 below LENGTH."
  (multiple-value-bind (y x) (floor which size)
    ;; Now both X and Y are below SIZE, and determine the quadrant.
    (loop for x% below size
          nconc (loop for y% below size
                      collect (+ x%
                                 (* x size) 
                                 (* y% length)
                                 (* y length size))))))

#+(or)
(quad-locations 0 3 9)


(defun set-cell-sets (game)
  (with-accessors ((size   sudoku-game:size)
                   (length sudoku-game:board-length)) game
    (loop for i below length
          collect (row-locations  i      length) into rows
          collect (col-locations  i      length) into cols
          collect (quad-locations i size length) into quads
          finally (setf 
                    (sudoku-game:row-set game) rows
                    (sudoku-game:col-set game) cols
                    (sudoku-game:area-set game) quads
                    (sudoku-game:all-sets game) 
                    (concatenate 'list rows cols quads)))))
                      

(defun find-sets-containing-location (location sets)
  (remove-if-not (lambda (set)
                   (member location set
                           :test #'=))
                 sets))


#+(or)
(get-cell-sets 3 9)

#+(or)
(find-sets-containing-location 0 
                               (apply #'concatenate 'list
                                      (get-cell-sets 3 9)))


(defun disallow-some (game sets allowed)
  "For each set cell in GAME, remove corresponding bits in ALLOWED."
  (loop with board  = (sudoku-game:board game)
        with length =  (sudoku-game:board-length game)
        with cells  = (* length length)
        for location below cells
        for cell = (row-major-aref board location)
        for bit = (bitnumber-from-value cell)
        if (not (is-blank game :value cell))
        do (loop for set in (find-sets-containing-location location sets) 
                 do (clear-bits-in-set allowed bit set))
        and do (setf (row-major-aref allowed location)
                     (mask-with-only-one-bit-at bit length)))
  allowed)


;;; Actual rules engine.


;; Simplest rule: only one of ALPHABET still allowed in a cell.
(defun rule-only-one-possibility (game allowed)
  (loop with board   = (sudoku-game:board game)
        with length  = (sudoku-game:board-length game)
        with cells   = (* length length)
        ;;
        for location below cells
        for cell     = (row-major-aref board   location)
        for mask     = (row-major-aref allowed location)
        for count    = (number-of-set-bits mask)
        ;;
        if (and (is-blank game :value cell)
                (= 1 (number-of-set-bits mask)))
        return (rule-got-value game location 
                             (value-from-bitnumber
                               (first-set-bit mask))
                             () ; all other cells, actually
                             "it has only a single possibility left.")))


(defun every-must-exist-in-set (game cell-set allowed)
  "Within CELL-SET, every one of ALPHABET must be used."
  (let* ((length  (sudoku-game:board-length game))
         (seen-at (make-array length
                              :element-type 'list
                              :initial-element ())))
    ;; count occurrences
    (loop for location in cell-set
          for mask     = (row-major-aref allowed location)
          do (dolist (bit (allowed-bits-in-mask mask))
                   (push location
                         (aref seen-at bit))))
    #+(or)
    (format *trace-output* "found seen:~% ~a~%" seen-at)
    ;; find elements with only one possible location
    (loop for bit below length
          for value = (value-from-bitnumber bit)
          for locations = (aref seen-at bit)
          for location = (first locations)
          if (and (= 1 (length locations))
                  (is-blank game :location location))
          return (rule-got-value game location value 
                               cell-set
                               "the value must exist within the set of cells ~a." cell-set))))

(defun rule-every-must-exist (game allowed)
  (loop for cell-set in (sudoku-game:all-sets game)
        thereis (every-must-exist-in-set game cell-set allowed)))


(defun restrict-by-neighbours (a-set b-set allowed)
  (let ((common (intersection   a-set b-set  :test #'=)))
    ;; If the intersection is a single cell, 
    ;; this is a degenerate case and comes out the same
    ;; as RULE-ONLY-ONE-POSSIBILITY.
    (when (> (length common) 1)
      (let* ((both   (union          a-set b-set  :test #'=))
             (a-only (set-difference a-set common :test #'=))
             (b-only (set-difference b-set common :test #'=)))
        (flet
          ((lookup (index)
             (row-major-aref allowed index)))
          (let ((bits-in-common   (reduce #'bit-ior (mapcar #'lookup common)))
                (bits-a-only      (reduce #'bit-ior (mapcar #'lookup a-only)))
                (bits-b-only      (reduce #'bit-ior (mapcar #'lookup b-only))))
            ;; Now we're looking for values that are allowed in COMMON,
            ;; but not in A-ONLY (or B-ONLY); these we can remove from
            ;; the other -only set.
            (loop for bit in (allowed-bits-in-mask bits-in-common)
              if (is-not-possible bits-a-only bit)
              do (clear-bits-in-set allowed bit b-only)
              and return (rule-outcome (list b-only both)
                                       "Remove ~a from cells ~a"
                                       (value-from-bitnumber bit)
                                       b-only)
              ;
              if (is-not-possible bits-b-only bit)
              do (clear-bits-in-set allowed bit a-only)
              and return (rule-outcome (list a-only both)
                                       "Remove ~a from cells ~a"
                                       (value-from-bitnumber bit)
                                       a-only)
              )))))))


(defun rule-restrict-by-neighbours (game allowed)
  "If within a cell-set A some value V is restricted to
   (ie. can only be in) a intersection
   with another cell-set B, then the other cells of B
   must not contain V.
   
   See here, for V=1, A being the quadrant, and B being the top row:

     yyy ... xxx     The x'es must not be 1; one of the y's must be.
     432 ... ...
     ... .1. ...
  "
  (with-accessors ((row-set  sudoku-game:row-set)
                   (col-set  sudoku-game:col-set)
                   (area-set sudoku-game:area-set)) game
    (loop for row in row-set
          do (loop for area in area-set
                   do (restrict-by-neighbours row area allowed)))
    (loop for col in col-set
          do (loop for area in area-set
                   do (restrict-by-neighbours col area allowed)))))



(defun subset-restrictions (cell-set allowed)
  (let ((same-allowed (make-hash-table :test #'equal)))
    (loop for location in cell-set
          do (push location
                   (gethash (row-major-aref allowed location)
                            same-allowed)))
    (loop for mask being the hash-key of same-allowed
          using (hash-value locations)
          for bits-set = (number-of-set-bits mask)
          if (= (length locations)
                bits-set)
          do (loop with other-cells = (set-difference cell-set
                                                      locations
                                                      :test #'eql)
                   for bit in (allowed-bits-in-mask mask)
                   collect (value-from-bitnumber bit) into disallowed-values
                   ;
                   sum (clear-bits-in-set allowed
                                            bit
                                            other-cells) into bits-cleared
                   finally (if (plusp bits-cleared)
                               (return-from subset-restrictions
                                 (rule-outcome (list cell-set)
                                               "Remove ~a from ~a"
                                               disallowed-values cell-set)))))))

(defun rule-full-subset-restricts-rest (game allowed)
  "If N cells of a quad allow only the same N numbers, then _no_ other 
   cells in that quad may have these - neither other in the same row or 
   col.
   Eg. if two cells end up with (2 7), then the rest must not have these."
  (loop for set in (sudoku-game:all-sets game)
        thereis (subset-restrictions set allowed)))


(defun solve-one-step (game)
  "Given GAME, returns a list of (REASON &rest USED-CELLS),
   or NIL if no progress could be made.
   USED-CELLS are one or more lists of cells to (differently) highlight."
  ;; TODO: should use ALPHABET and BLANK
  (with-accessors ((size     sudoku-game:size)
                   (width    sudoku-game:board-length)
                   (allowed  sudoku-game:allowed)
                   (all-sets sudoku-game:all-sets)) game
    ;;
    (unless all-sets
      (set-cell-sets game))
    ;;
    (unless allowed
      (setf allowed (make-allowed-array width)))
    ;; Newly set values may prohibit some values in other cells
    (disallow-some game all-sets allowed)
    ;; now try to solve.
    (or (rule-only-one-possibility        game allowed)
        (rule-every-must-exist            game allowed)
        (rule-restrict-by-neighbours      game allowed)
        (rule-full-subset-restricts-rest  game allowed))))


#+(or)
(solve-one-step (sudoku-game:make-classic-game
                  (aref sudoku-example-games:*games* 2)))
