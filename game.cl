
(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

;; Print out a ASCII representation of the game board
(defun print-board (board)
  (let* ((convert-to-letter (lambda (v)
          (cond ((equal v 1) "0")
                ((equal v 10) "X")
                (t " "))))
        (print-row (lambda (x y z)
          (format t "~&  ~A | ~A | ~A"
            (funcall convert-to-letter x)
            (funcall convert-to-letter y)
            (funcall convert-to-letter z)))))

  (format t "~%")
  (funcall print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& ___________")
  (funcall print-row
    (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& ___________")
  (funcall print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%")))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *opponent1* 10)
(setf *opponent2* 1)

(setf *triplets*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
    *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *opponent1*) sums)
        (member (* 3 *opponent2*) sums))))


(defun opponent-move-p1 (board)
  (format t "~&Player 1s turn")
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent1*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&Player 1 Wins!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (*multiplayer-mode*
           (opponent-move-p2 new-board))
          (t (computer-move new-board)))))

(defun opponent-move-p2 (board)
  (format t "~&Player 2s turn")
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent2*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&Player 2 Wins!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move-p1 new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
               "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *opponent2* pos board)))
    (format t "~&Computers move: ~S" pos)
    (format t "~&Computers strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&Computer wins!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move-p1 new-board)))))

(defun choose-best-move (board)
  (if *hard-mode-on*
    (expert-move-strategy board)
  (random-move-strategy board)))

(defun expert-move-strategy (board)
  (list (pick-random-empty-position board)
        "expert move"))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
      (pick-random-empty-position board))))

(defun play-one-game ()
  (setf *multiplayer-mode* (not (y-or-n-p "Play against the computer?")))
  (if (not *multiplayer-mode*)
    (progn (setf *hard-mode-on* (y-or-n-p "Would you like to play on expert-mode?"))
    (if (y-or-n-p "Would you like to go first?")
      (opponent-move-p1 (make-board))
    (computer-move (make-board))))
  (opponent-move-p1 (make-board))))
