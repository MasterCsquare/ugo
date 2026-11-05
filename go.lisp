(in-package :ugo)

(defparameter *size* 20)
(defparameter *dot*  '·)

(defparameter *white* '●)
(defparameter *black* '○)

(defparameter *board*
  (make-array `(,*size* ,*size*)
              :initial-element *dot*))

(defun setb (a b c)
  (setf (aref *board* a b) c))

(setb 0 0 0)
(setb 0 1 1)
(setb 0 2 2)
(setb 0 3 3)
(setb 0 4 4)
(setb 0 5 5)
(setb 0 6 6)
(setb 0 7 7)
(setb 0 8 8)
(setb 0 9 9)
(setb 0 10 'a)
(setb 0 11 'b)
(setb 0 12 'c)
(setb 0 13 'd)
(setb 0 14 'e)
(setb 0 15 'f)
(setb 0 16 'g)
(setb 0 17 'h)
(setb 0 18 'i)
(setb 0 19 'j)

(setb 1 0 1)
(setb 2 0 2)
(setb 3 0 3)
(setb 4 0 4)
(setb 5 0 5)
(setb 6 0 6)
(setb 7 0 7)
(setb 8 0 8)
(setb 9 0 9)
(setb 10 0 'a)
(setb 11 0 'b)
(setb 12 0 'c)
(setb 13 0 'd)
(setb 14 0 'e)
(setb 15 0 'f)
(setb 16 0 'g)
(setb 17 0 'h)
(setb 18 0 'i)
(setb 19 0 'j)

(defparameter transformation-table
  '((1 . 1)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 5)
    (6 . 6)
    (7 . 7)
    (8 . 8)
    (9 . 9)
    (a . 10)
    (b . 11)
    (c . 12)
    (d . 13)
    (e . 14)
    (f . 15)
    (g . 16)
    (h . 17)
    (i . 18)
    (j . 19)))

(defun transformation (index)
  (dolist (record transformation-table)
    (if (eq index (car record))
        (return (cdr record)))))

(defun move (element x y)
  (setb y x element)
  *board*)

(defmacro w (x y)
  `(move *white*
         (transformation ',x)
         (transformation ',y)))

(defmacro b (x y)
  `(move *black*
         (transformation ',x)
         (transformation ',y)))

(defmacro d (x y)
  `(move *dot*
         (transformation ',x)
         (transformation ',y)))

;;;; 数气制 (liberty-counting rule)
;;;;
;;;; 终局计分：
;;;; - 按四连通把同色棋子分成块，每块的气 = 与之正交相邻的空点。
;;;; - 一方的气为其所有块的气的并集（同一点只算一次）。
;;;; - 只与一方棋子相邻的空点归该方，得 1 气。
;;;; - 同时与黑白双方相邻的空点（公气）双方都不计；它对双方加的数相同，
;;;;   本来也不会影响胜负判定。
;;;; - 没有气的棋块是死子，终局时在副本上同时提掉，再计算双方气数。
;;;; - 气多者胜，气相等为和棋。

(defun inside-board-p (x y)
  "Whether (X, Y) is a playable point (the labels on row/column 0 are not)."
  (and (<= 1 x) (< x *size*)
       (<= 1 y) (< y *size*)))

(defun board-ref (x y)
  (aref *board* y x))

(defun copy-board (board)
  (let ((copy (make-array (array-dimensions board))))
    (dotimes (i (array-total-size board) copy)
      (setf (row-major-aref copy i)
            (row-major-aref board i)))))

(defun neighbors-of (x y)
  (loop for (nx ny) in (list (list (1+ x) y)
                             (list (1- x) y)
                             (list x (1+ y))
                             (list x (1- y)))
        when (inside-board-p nx ny)
        collect (cons nx ny)))

(defun find-group (x y seen)
  "Flood-fill the group of stones at (X, Y).
Return (values color stones liberties), marking visited points in SEEN."
  (let ((color (board-ref x y))
        (stack (list (cons x y)))
        (stones '())
        (liberties '()))
    (loop while stack do
             (let* ((point (pop stack))
                    (px (car point))
                    (py (cdr point)))
               (unless (aref seen py px)
                 (setf (aref seen py px) t)
                 (push point stones)
                 (dolist (n (neighbors-of px py))
                   (let ((nx (car n))
                         (ny (cdr n)))
                     (cond ((eq (board-ref nx ny) color)
                            (unless (aref seen ny nx)
                              (push n stack)))
                           ((eq (board-ref nx ny) *dot*)
                            (pushnew n liberties :test #'equal))))))))
    (values color (nreverse stones) liberties)))

(defun collect-groups ()
  "Return plists (:color :stones :liberties) for every group on *board*."
  (let ((seen (make-array (list *size* *size*) :initial-element nil))
        (groups '()))
    (loop for y from 1 below *size* do
             (loop for x from 1 below *size* do
                      (when (and (not (aref seen y x))
                                 (not (eq (board-ref x y) *dot*)))
                        (multiple-value-bind (color stones liberties)
                            (find-group x y seen)
                          (push (list :color color
                                      :stones stones
                                      :liberties liberties)
                                groups)))))
    (nreverse groups)))

(defun liberty-set (color groups)
  "Union of the liberty points of all groups of COLOR."
  (let ((set '()))
    (dolist (group groups)
      (when (eq (getf group :color) color)
        (dolist (point (getf group :liberties))
          (pushnew point set :test #'equal))))
    set))

(defun remove-dead-groups ()
  "Remove every zero-liberty group from *board* simultaneously.
Return the list of removed points."
  (let ((dead '()))
    (dolist (group (collect-groups))
      (when (null (getf group :liberties))
        (dolist (point (getf group :stones))
          (push point dead))))
    (dolist (point dead)
      (setf (aref *board* (cdr point) (car point)) *dot*))
    (nreverse dead)))

(defun score-qi-of (board &key (copy t))
  "Score BOARD under the liberty-counting rule.
When COPY is true the board is left intact; otherwise dead groups are
removed directly from BOARD.  Return (values white-qi black-qi)."
  (let ((*board* (if copy (copy-board board) board)))
    (remove-dead-groups)
    (let* ((groups (collect-groups))
           (white (liberty-set *white* groups))
           (black (liberty-set *black* groups))
           (dame (intersection white black :test #'equal)))
      (values (length (set-difference white dame :test #'equal))
              (length (set-difference black dame :test #'equal))))))

(defun score-qi ()
  "Score the current position.  *board* is left intact.
Return (values white-qi black-qi)."
  (score-qi-of *board*))

(defun qi-winner ()
  "Return :white, :black or :draw under the liberty-counting rule."
  (multiple-value-bind (white black) (score-qi)
    (cond ((> white black) :white)
          ((< white black) :black)
          (t :draw))))

(defun show-qi-score ()
  "Count both sides' qi and print the result."
  (multiple-value-bind (white black) (score-qi)
    (format t "~&数气制：白 ~d 气，黑 ~d 气 —— ~a~%"
            white black
            (cond ((> white black) "白胜")
                  ((< white black) "黑胜")
                  (t "和棋")))
    (values white black)))

*board*
