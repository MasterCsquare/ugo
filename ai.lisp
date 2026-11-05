(in-package :ugo)

;;;; A classical Monte Carlo tree search (UCT) player for 数气制.
;;;;
;;;; There is no neural network and nothing is learned: the tree is guided by
;;;; random light playouts whose value is the exact liberty-counting score.
;;;; This is the pre-AlphaGo style of computer Go (UCT, Kocsis & Szepesvari
;;;; 2006), just with this variant's scoring rule plugged in as the payoff.

(defparameter *ai-random-state* (make-random-state t))
(defparameter *ai-simulations* 5000)
(defparameter *ai-time-limit* 3.0)
(defparameter *uct-c* 1.414)
(defparameter *rollout-depth* 50)
(defparameter *rollout-samples* 8)
(defparameter *rollout-policy* :heuristic
  "Either :RANDOM or :HEURISTIC (a light local policy, no search).")
(defparameter *score-scale* 12.0
  "Controls how strongly a qi margin is pushed into [0,1] by the tanh.")
(defparameter *passes-to-end* 2
  "Consecutive passes that end the game.  Set to 1 to remove the forced-draw
loophole: a player who is ahead can then stop the game on their own turn.")

(defun other-color (color)
  (if (eq color *white*) *black* *white*))

(defun random-element (list)
  (nth (random (length list) *ai-random-state*) list))

(defun shuffle-vector! (vector)
  (loop for i from (1- (length vector)) downto 1 do
    (let ((j (random (1+ i) *ai-random-state*)))
      (rotatef (aref vector i) (aref vector j))))
  vector)

(defun empty-move-vector (board)
  "All empty playable points of BOARD, as a fresh simple vector of conses."
  (let* ((n-points (* (1- *size*) (1- *size*)))
         (moves (make-array n-points))
         (count 0))
    (loop for y from 1 below *size* do
      (loop for x from 1 below *size* do
        (when (eq (aref board y x) *dot*)
          (setf (aref moves count) (cons x y))
          (incf count))))
    (if (= count n-points)
        moves
        (subseq moves 0 count))))

(defun empty-moves (board)
  "All empty playable points of BOARD as (X . Y) conses."
  (coerce (empty-move-vector board) 'list))

(defun board-after-move (board color move)
  "Return a copy of BOARD with COLOR played at MOVE."
  (let ((new (copy-board board)))
    (setf (aref new (cdr move) (car move)) color)
    new))

(defun qi-margin (board color &key (copy t))
  "COLOR's qi minus the opponent's qi on BOARD."
  (multiple-value-bind (white black) (score-qi-of board :copy copy)
    (if (eq color *white*) (- white black) (- black white))))

(defun qi-utility (board color &key (copy t))
  "Map the qi margin into [0,1] from COLOR's point of view.
Used as a heuristic value at rollout cutoffs."
  (+ 0.5d0
     (* 0.5d0 (tanh (/ (qi-margin board color :copy copy)
                       *score-scale*)))))

(defun qi-outcome (board color)
  "The actual game result from COLOR's point of view: 1, 1/2 or 0."
  (multiple-value-bind (white black) (score-qi-of board)
    (cond ((> white black) (if (eq color *white*) 1d0 0d0))
          ((< white black) (if (eq color *black*) 1d0 0d0))
          (t 0.5d0))))

;;;; Tree nodes ------------------------------------------------------------

(defstruct (ai-node (:conc-name node-))
  board
  to-move
  passes
  parent
  action
  children
  (untried #())
  (untried-count 0)
  (visits 0)
  (value 0.0d0)
  (terminal nil))

(defun make-node (board to-move passes &key parent action)
  (if (>= passes *passes-to-end*)
      (make-ai-node :board board
                    :to-move to-move
                    :passes passes
                    :parent parent
                    :action action
                    :terminal t)
      (let* ((moves (empty-move-vector board))
             (n (length moves)))
        (if (zerop n)
            (make-ai-node :board board
                          :to-move to-move
                          :passes passes
                          :parent parent
                          :action action
                          :terminal t)
            (let ((actions (make-array (1+ n))))
              (setf (aref actions 0) :pass)
              (replace actions moves :start1 1)
              (make-ai-node :board board
                            :to-move to-move
                            :passes passes
                            :parent parent
                            :action action
                            :untried (shuffle-vector! actions)
                            :untried-count (1+ n)))))))

(defun expand-node (node)
  "Turn one untried action of NODE into a child, and return that child."
  (let* ((count (decf (node-untried-count node)))
         (action (aref (node-untried node) count))
         (color (node-to-move node))
         (child-board (if (eq action :pass)
                          (node-board node)
                          (board-after-move (node-board node) color action)))
         (child (make-node child-board
                           (other-color color)
                           (if (eq action :pass)
                               (1+ (node-passes node))
                               0)
                           :parent node
                           :action action)))
    (push child (node-children node))
    child))

(defun select-child (node root-color)
  "Choose a child by UCB, from the perspective of the player to move at NODE."
  (let ((best nil)
        (best-score most-negative-double-float)
        (parent-visits (node-visits node))
        (maximize (eq (node-to-move node) root-color)))
    (dolist (child (node-children node) best)
      (let* ((mean (/ (node-value child) (node-visits child)))
             (exploit (if maximize mean (- 1.0d0 mean)))
             (explore (* *uct-c*
                         (sqrt (/ (log (max 2 parent-visits))
                                  (node-visits child)))))
             (score (+ exploit explore)))
        (when (> score best-score)
          (setf best child
                best-score score))))))

(defun tree-policy (node root-color)
  (loop
    (when (node-terminal node)
      (return node))
    (when (plusp (node-untried-count node))
      (return (expand-node node)))
    (setf node (select-child node root-color))))

;;;; Playouts --------------------------------------------------------------

(defun local-move-score (board color x y)
  "A cheap, purely local move heuristic used by the rollout policy."
  (let ((score 0)
        (liberties 0))
    (dolist (n (neighbors-of x y))
      (let* ((nx (car n))
             (ny (cdr n))
             (cell (aref board ny nx)))
        (cond ((eq cell *dot*)
               (incf liberties)
               (incf score))
              ((eq cell color)
               (incf score 1))
              (t
               (incf score 2)
               ;; Approximate atari: this enemy stone has one empty neighbor.
               (let ((enemy-liberties
                       (loop for m in (neighbors-of nx ny)
                             count (eq (aref board (cdr m) (car m)) *dot*))))
                 (when (= enemy-liberties 1)
                   (incf score 6)))))))
    (cond ((zerop liberties) (decf score 20))
          ((= liberties 1) (decf score 2)))
    score))

(defun choose-rollout-index (board empties count color)
  "Sample a few empty points and return the index of the locally best one."
  (let ((best-index 0)
        (best-score most-negative-fixnum))
    (loop repeat (min *rollout-samples* count) do
      (let* ((i (random count *ai-random-state*))
             (move (aref empties i))
             (score (local-move-score board color (car move) (cdr move))))
        (when (> score best-score)
          (setf best-score score
                best-index i))))
    best-index))

(defun rollout (board color root-color)
  "Play *ROLLOUT-DEPTH* light moves from BOARD, then score the position."
  (let* ((sim (copy-board board))
         (empties (empty-move-vector board))
         (count (length empties))
         (turn color))
    (loop repeat *rollout-depth* do
      (when (zerop count)
        (return))
      (let* ((i (if (eq *rollout-policy* :random)
                    (random count *ai-random-state*)
                    (choose-rollout-index sim empties count turn)))
             (move (aref empties i)))
        (setf (aref empties i) (aref empties (1- count)))
        (decf count)
        (setf (aref sim (cdr move) (car move)) turn)
        (setf turn (other-color turn))))
    (qi-utility sim root-color :copy nil)))

(defun simulate (node root-color)
  (if (node-terminal node)
      (qi-outcome (node-board node) root-color)
      (rollout (node-board node) (node-to-move node) root-color)))

(defun backpropagate (node value)
  (loop while node do
    (incf (node-visits node))
    (incf (node-value node) value)
    (setf node (node-parent node))))

;;;; Search ---------------------------------------------------------------

(defun best-child (node)
  "Most visited child; ties broken by mean value."
  (let ((best nil))
    (dolist (child (node-children node) best)
      (when (or (null best)
                (> (node-visits child) (node-visits best))
                (and (= (node-visits child) (node-visits best))
                     (> (/ (node-value child) (node-visits child))
                        (/ (node-value best) (node-visits best)))))
        (setf best child)))))

(defun mcts-best-move (board color &key (simulations *ai-simulations*)
                                      (time-limit *ai-time-limit*)
                                      (passes 0))
  "Return the UCT-selected move for COLOR as (X . Y), or :pass.
Only the qi-counting score is used as the payoff."
  (let* ((root (make-node (copy-board board) color passes))
         (deadline (when time-limit
                     (+ (get-internal-real-time)
                        (round (* time-limit internal-time-units-per-second))))))
    (if (node-terminal root)
        :pass
        (progn
          (loop repeat simulations do
            (when (and deadline
                       (>= (get-internal-real-time) deadline))
              (return))
            (let ((leaf (tree-policy root color)))
              (backpropagate leaf (simulate leaf color))))
          (let ((child (best-child root)))
            (cond (child (node-action child))
                  ((empty-moves board) (random-element (empty-moves board)))
                  (t :pass)))))))

;;;; Convenience players ---------------------------------------------------

(defparameter *ai-engine* :mcts
  "Default engine: :MCTS, :MINIMAX or :GREEDY.")

(defun advisable-pass-p (board color)
  "Passing is an immediate win when a single pass ends the game and COLOR
is ahead."
  (and (= *passes-to-end* 1)
       (> (qi-margin board color) 0)))

(defun ai-best-move (board color &key (engine *ai-engine*)
                                   (simulations *ai-simulations*)
                                   (time-limit *ai-time-limit*)
                                   (passes 0))
  "Ask ENGINE for a move for COLOR on BOARD."
  (if (advisable-pass-p board color)
      :pass
      (case engine
        (:mcts (mcts-best-move board color
                               :simulations simulations
                               :time-limit time-limit
                               :passes passes))
        (:minimax (minimax-best-move board color))
        (:greedy (greedy-best-move board color))
        (otherwise (error "Unknown AI engine: ~s" engine)))))

(defun ai-move (color &key (engine *ai-engine*)
                          (simulations *ai-simulations*)
                          (time-limit *ai-time-limit*)
                          (passes 0)
                          (play t))
  "Pick a move for COLOR on *board* with ENGINE.
Return the move as (X . Y), or :pass.  With PLAY true the stone is also
placed on *board* (a pass changes nothing)."
  (let ((move (ai-best-move *board* color
                            :engine engine
                            :simulations simulations
                            :time-limit time-limit
                            :passes passes)))
    (when (and play (consp move))
      (setf (aref *board* (cdr move) (car move)) color))
    move))

(defun greedy-best-move (board color)
  "A one-ply baseline: maximize the immediate qi margin after the move."
  (let ((best-value most-negative-double-float)
        (best-moves '()))
    (dolist (move (empty-moves board))
      (let* ((next (board-after-move board color move))
             (value (float (qi-margin next color) 1d0)))
        (cond ((> value best-value)
               (setf best-value value
                     best-moves (list move)))
              ((= value best-value)
               (push move best-moves)))))
    (cond ((null best-moves) :pass)
          ((null (rest best-moves)) (first best-moves))
          (t (random-element best-moves)))))

(defun rank-moves (board color root-color limit)
  "Return the LIMIT moves that are statically best for COLOR (as seen from
ROOT-COLOR's margin), using exact scoring for every candidate.
The candidate order is shuffled first so that ties are broken randomly."
  (let ((scored '())
        (moves (shuffle-vector! (empty-move-vector board))))
    (loop for move across moves do
      (push (cons move
                  (float (qi-margin (board-after-move board color move)
                                    root-color)
                         1d0))
            scored))
    (let ((ordered (stable-sort scored
                                (if (eq color root-color) #'> #'<)
                                :key #'cdr)))
      (mapcar #'car (subseq ordered 0 (min limit (length ordered)))))))

(defun beam-value (board color depth root-color beam)
  "Minimax value of BOARD from ROOT-COLOR's point of view.
Only the BEAM statically best moves are explored at each node."
  (if (<= depth 0)
      (float (qi-margin board root-color) 1d0)
      (let ((moves (rank-moves board color root-color beam)))
        (if (null moves)
            (float (qi-margin board root-color) 1d0)
            (let ((values
                    (loop for move in moves
                          collect (beam-value
                                   (board-after-move board color move)
                                   (other-color color)
                                   (1- depth)
                                   root-color
                                   beam))))
              (if (eq color root-color)
                  (reduce #'max values)
                  (reduce #'min values)))))))

(defun minimax-best-move (board color &key (depth 3) (root-beam 24) (beam 8))
  "Beam minimax player.  DEPTH plies, ROOT-BEAM candidates at the root and
BEAM candidates deeper.  Moves are ordered with the exact qi margin, and
leaves are scored with the actual 数气制 score."
  (let ((moves (rank-moves board color color root-beam))
        (best-value most-negative-double-float)
        (best-moves '()))
    (cond ((null moves) :pass)
          (t
           (dolist (move moves)
             (let ((value (beam-value (board-after-move board color move)
                                      (other-color color)
                                      (1- depth)
                                      color
                                      beam)))
               (cond ((> value best-value)
                      (setf best-value value
                            best-moves (list move)))
                     ((= value best-value)
                      (push move best-moves)))))
           (if (null (rest best-moves))
               (first best-moves)
               (random-element best-moves))))))

(defun ai-self-play (&key (board (copy-board *board*))
                          (engine *ai-engine*)
                          (white-simulations 400)
                          (black-simulations 400)
                          (time-limit 1.0)
                          (max-moves 200)
                          (verbose t))
  "Let two AI players finish a game (or play MAX-MOVES) and print the score."
  (let ((passes 0)
        (moves 0)
        (turn *white*))
    (loop
      (when (>= passes *passes-to-end*)
        (return))
      (when (>= moves max-moves)
        (return))
      (let ((move (ai-best-move board turn
                                :engine engine
                                :simulations (if (eq turn *white*)
                                                 white-simulations
                                                 black-simulations)
                                :time-limit time-limit
                                :passes passes)))
        (when verbose
          (format t "~&~2d. ~a ~a~%"
                  (1+ moves)
                  (if (eq turn *white*) "白" "黑")
                  (if (eq move :pass)
                      "pass"
                      (format nil "(~d,~d)" (car move) (cdr move)))))
        (if (eq move :pass)
            (incf passes)
            (progn
              (setf (aref board (cdr move) (car move)) turn)
              (setf passes 0)
              (incf moves)))
        (setf turn (other-color turn))))
    (multiple-value-bind (white black) (score-qi-of board)
      (format t "~&数气制：白 ~d 气，黑 ~d 气 —— ~a~%"
              white black
              (cond ((> white black) "白胜")
                    ((< white black) "黑胜")
                    (t "和棋")))
      (values white black))))
