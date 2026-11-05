(in-package :ugo)

;;;; Practice mode: an interactive 数气制 game against the AI.

(defun make-practice-board ()
  "Return an empty board that keeps the coordinate labels from *board*."
  (let ((board (copy-board *board*)))
    (loop for y from 1 below *size* do
      (loop for x from 1 below *size* do
        (setf (aref board y x) *dot*)))
    board))

(defun practice-color-name (color)
  (if (eq color *white*) "白" "黑"))

(defun practice-resolve-color (color)
  (cond ((or (eq color :white) (eq color *white*)) *white*)
        ((or (eq color :black) (eq color *black*)) *black*)
        (t (error "Unknown color ~s: use *WHITE*, *BLACK*, :WHITE or :BLACK."
                  color))))

(defun practice-label (label)
  "The coordinate label as the player should type it (lower-case a-j)."
  (string-downcase (format nil "~a" label)))

(defun print-practice-board (board &key (stream *standard-output*))
  (format stream "~&   ")
  (loop for x from 1 below *size* do
    (format stream "~a " (practice-label (aref board 0 x))))
  (terpri stream)
  (loop for y from 1 below *size* do
    (format stream "~2a " (practice-label (aref board y 0)))
    (loop for x from 1 below *size* do
      (format stream "~a " (aref board y x)))
    (terpri stream))
  (finish-output stream))

(defun practice-empty-p (board)
  (loop for y from 1 below *size* thereis
    (loop for x from 1 below *size* thereis
      (eq (aref board y x) *dot*))))

(defun print-practice-score (board human &key (stream *standard-output*))
  (multiple-value-bind (white black) (score-qi-of board)
    (let ((margin (if (eq human *white*) (- white black) (- black white))))
      (format stream "~&当前数气：白 ~d，黑 ~d" white black)
      (cond ((plusp margin)
             (format stream "（你领先 ~d 气）~%" margin))
            ((minusp margin)
             (format stream "（你落后 ~d 气）~%" (- margin)))
            (t
             (format stream "（双方持平）~%")))
      (finish-output stream))))

(defun split-practice-input (line)
  (let ((tokens '())
        (start nil))
    (loop for i from 0 below (length line)
          for ch = (char line i)
          do (if (member ch '(#\Space #\Tab #\Return #\,))
                 (when start
                   (push (subseq line start i) tokens)
                   (setf start nil))
                 (unless start
                   (setf start i)))
          finally (when start
                    (push (subseq line start) tokens)))
    (nreverse tokens)))

(defun parse-practice-coordinate (token)
  "Parse a column/row label: 1-9, a-j (case-insensitive), or a number 1-19."
  (when (plusp (length token))
    (cond ((every #'digit-char-p token)
           (let ((n (parse-integer token)))
             (and (<= 1 n) (< n *size*) n)))
          ((= (length token) 1)
           (let ((ch (char-downcase (char token 0))))
             (cond ((char<= #\1 ch #\9)
                    (- (char-code ch) (char-code #\0)))
                   ((char<= #\a ch #\j)
                    (+ 10 (- (char-code ch) (char-code #\a))))
                   (t nil))))
          (t nil))))

(defun parse-practice-command (line)
  "Return :PASS, :QUIT, :HELP, :INVALID, or a move as (X . Y)."
  (let* ((tokens (split-practice-input line))
         (word (if tokens (string-downcase (first tokens)) "")))
    (cond ((null tokens) :invalid)
          ((and (null (rest tokens))
                (member word '("p" "pass" "过" "停") :test #'string=))
           :pass)
          ((and (null (rest tokens))
                (member word '("q" "quit" "exit" "退出") :test #'string=))
           :quit)
          ((and (null (rest tokens))
                (member word '("h" "help" "?" "帮助") :test #'string=))
           :help)
          ((= (length tokens) 2)
           (let ((x (parse-practice-coordinate (first tokens)))
                 (y (parse-practice-coordinate (second tokens))))
             (if (and x y)
                 (cons x y)
                 :invalid)))
          (t :invalid))))

(defun print-practice-help (stream)
  (format stream "~&命令：~%")
  (format stream "  坐标：列 行，例如 9 9 或 e 5~%")
  (format stream "        标号为 1-9 和 a-j（a=10, j=19），也可直接输入 1-19 的数字~%")
  (format stream "  pass / p：停一手（~a）~%"
          (if (= *passes-to-end* 1)
              "一次 pass 即终局"
              (format nil "连续 ~d 次 pass 终局" *passes-to-end*)))
  (format stream "  help / h：显示帮助~%")
  (format stream "  quit / q：退出~%")
  (format stream "  自由落子，不提子；终局时同时提掉双方无气死子再数气。~%")
  (finish-output stream))

(defun ask-practice-move (board human input output)
  (loop
    (format output "~&你（~a ~a）走棋，输入 列 行（pass/help/quit）： "
            (practice-color-name human) human)
    (finish-output output)
    (let ((line (read-line input nil :eof)))
      (when (eq line :eof)
        (format output "~&输入结束。~%")
        (return :quit))
      (let ((command (parse-practice-command line)))
        (case command
          (:pass (return :pass))
          (:quit (return :quit))
          (:help (print-practice-help output))
          (:invalid
           (format output "~&无效输入，请重试。~%")
           (finish-output output))
          (otherwise
           (let ((x (car command))
                 (y (cdr command)))
             (cond ((not (inside-board-p x y))
                    (format output "~&坐标超出棋盘。~%")
                    (finish-output output))
                   ((not (eq (aref board y x) *dot*))
                    (format output "~&该点已有棋子。~%")
                    (finish-output output))
                   (t
                    (return command))))))))))

(defun practice-ai-move (board ai passes engine simulations time-limit output)
  (format output "~&AI（~a ~a）思考中...~%"
          (practice-color-name ai) ai)
  (finish-output output)
  (if (and (>= passes (1- *passes-to-end*))
           (> (qi-margin board ai) 0))
      :pass
      (ai-best-move board ai
                    :engine engine
                    :simulations simulations
                    :time-limit time-limit
                    :passes passes)))

(defun practice (&key (human *black*)
                      (engine *ai-engine*)
                      (simulations *ai-simulations*)
                      (time-limit *ai-time-limit*)
                      (passes-to-end 1)
                      (input *standard-input*)
                      (output *standard-output*))
  "Play an interactive 数气制 game against the AI.

HUMAN is *BLACK* (the default; black moves first) or *WHITE*; :BLACK and
:WHITE are accepted as well.  Enter a move as column and row, for example
`9 9` or `e 5`; labels are 1-9 and a-j (10-19), and plain numbers 10-19 also
work.  Commands are PASS (P), HELP (H) and QUIT (Q).

The game ends after PASSES-TO-END consecutive passes (default 1: a single
pass ends the game) or when the board is full.  The default single-pass
ending avoids the forced 0:0 draw described in the README; the AI passes as
soon as it is ahead, and the human may do the same.

Return (values BOARD WHITE-QI BLACK-QI).  The qi values are NIL if the
player quit before the end."
  (unless (and (integerp passes-to-end) (plusp passes-to-end))
    (error "PASSES-TO-END must be a positive integer, not ~s." passes-to-end))
  (let* ((human (practice-resolve-color human))
         (ai (other-color human))
         (board (make-practice-board))
         (*passes-to-end* passes-to-end)
         (passes 0)
         (turn *black*)
         (moves 0)
         (at-end nil)
         (quit nil))
    (format output "~&数气制练习模式。你执~a（~a），AI 执~a（~a）。黑先。~%"
            (practice-color-name human) human
            (practice-color-name ai) ai)
    (print-practice-help output)
    (loop until (or at-end quit)
          do (cond
               ((not (practice-empty-p board))
                (setf at-end :full))
               (t
                (print-practice-board board :stream output)
                (print-practice-score board human :stream output)
                (if (eq turn human)
                    (let ((move (ask-practice-move board human input output)))
                      (case move
                        (:quit (setf quit t))
                        (:pass
                         (incf passes)
                         (format output "~&你 pass（连续 ~d 次）。~%" passes)
                         (when (>= passes *passes-to-end*)
                           (setf at-end :ended))
                         (setf turn ai))
                        (otherwise
                         (setf (aref board (cdr move) (car move)) human)
                         (setf passes 0)
                         (incf moves)
                         (format output "~&你落子：~a ~a。~%"
                                 (practice-label (aref board 0 (car move)))
                                 (practice-label (aref board (cdr move) 0)))
                         (setf turn ai))))
                    (let ((move (practice-ai-move board ai passes engine
                                                  simulations time-limit output)))
                      (if (eq move :pass)
                          (progn
                            (incf passes)
                            (format output "~&AI pass（连续 ~d 次）。~%" passes)
                            (when (>= passes *passes-to-end*)
                              (setf at-end :ended))
                            (setf turn human))
                          (progn
                            (setf (aref board (cdr move) (car move)) ai)
                            (setf passes 0)
                            (incf moves)
                            (format output "~&AI 落子：~a ~a。~%"
                                    (practice-label (aref board 0 (car move)))
                                    (practice-label (aref board (cdr move) 0)))
                            (setf turn human))))))))
    (if quit
        (progn
          (format output "~&已退出本局（未计分）。~%")
          (values board nil nil))
        (progn
          (format output "~&~a~%"
                  (if (eq at-end :full) "棋盘已满，终局。" "终局。"))
          (multiple-value-bind (white black) (score-qi-of board :copy nil)
            (print-practice-board board :stream output)
            (format output "~&共 ~d 手（不含 pass）。最终数气：白 ~d 气，黑 ~d 气。~%"
                    moves white black)
            (format output "你执~a（~a），AI 执~a（~a）。~%"
                    (practice-color-name human) human
                    (practice-color-name ai) ai)
            (let ((winner (cond ((> white black) *white*)
                                ((< white black) *black*)
                                (t nil))))
              (cond ((null winner)
                     (format output "结果：和棋。~%"))
                    ((eq winner human)
                     (format output "结果：你赢了！~%"))
                    (t
                     (format output "结果：AI 赢了。~%")))
              (finish-output output))
            (values board white black))))))
