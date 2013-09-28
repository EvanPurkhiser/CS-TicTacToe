;; These are the opponent identifiers
(setf *opponent1* 10)
(setf *opponent2* 1)

;; This is a list representing the 3 in a row positions on a board that are
;; consider wining rows
(setf *triplets*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)))

;; Setup the playing board filled with zeros to start. The playing board will be
;; filled with the *opponent1* and *opponent2* constants as the game progresses.
;; We can use these values to determine which player has won the game
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

;; Read from stdin a legal move for the current board. This ensure the move fits
;; in the board space, and isn't already taken by any of the players
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

;; Update the board with a players move
(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

;; Test if the board no longer has any free spots left on it. We can do this by
;; simply looking for any position on the board that is still set to the blank
;; value of 0
(defun board-full-p (board)
  (not (member 0 board)))

;; Determine the sum for a single 3-in-a-row on the game board
(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

;; Compute the triplet sums for the entire board
(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
    (sum-triplet board triplet))
    *triplets*))

;; Find a position on the board that will continue or complete a row. Use the
;; num-in-row binding to specify how many boxes in the row should already have
;; been taken by the given opponent
(defun find-in-a-row (board opponent num-in-row)
  ;; Find the empty position on the 2-in-a-row line
  (car (remove-if-not #'(lambda(pos) (eq 0 (nth pos board)))
  ;; Find the winning line (triplet)
  (car (remove-if-not #'(lambda(triplet)
      (eq (sum-triplet board triplet) (* num-in-row opponent))) *triplets*)))))

;; Determine if someone has won the game. We can do this by calculating the sums
;; for each of the boards triplets (three in a row spaces). If one of the
;; players has won then the sum should be 3x the value of the players ID. For
;; example, opponent1 has a id of 10, so if we sum a triplet to 30 then we know
;; that the player has won the game by getting 3-in-a-row
(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *opponent1*) sums)
        (member (* 3 *opponent2*) sums))))

;;; Opponent movement functions...
;;; To be honest, this should _really_ be combined into a single function, but
;;; I'm still struggling a little bit with lisp and haven't totally got the
;;; hang of it, so most of the patterns I'm used to won't really work here.

;; Allow the first player to make his move and determine the game state. This
;; will then ask the next player to make a move. If in a multi player game then
;; we will pass it off to the second player move function, if not then let the
;; computer make a move
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

;; Allow the second player to make a move then pass it back to the first player
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

;; Let the computer make a move then pass it back to the first player
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

;;; Computer AI functionality

;; Determine the best move to make. If the player has enabled hard mode then
;; pass it off to the expert which will delegate to some different strategies
;; based on the board status. If we're on easy mode then just make a random move.
;;
;; We will always eject back a list where the first element is the move position
;; and the second element is a string describing the strategy that was used for
;; the move
(defun choose-best-move (board)
  (if *hard-mode-on*
    (expert-move-strategy board)
  (random-move-strategy board)))

;; Select a random move
(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "Random move"))

;; Pick a random position on the board. This is recessive, so it will keep
;; looking for positions until it can find a free one
(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
      (pick-random-empty-position board))))

;; Expert move making mode. When the game is played in this mode then four
;; different strategies will be tried, in order of importance.
(defun expert-move-strategy (board)
  (cond ;; If possible try and win the game
        ((winning-move-strategy board))
        ;; If we can't win make sure the opponent can't
        ((defensive-move-strategy board))
        ;; If the opponent can't win be offensive (2-in-a-row)
        ((offensive-move-strategy board))
        ;; Can't be offensive, make a random move
        ((random-move-strategy board))))

;; Look for a position on the board where we can take the win
(defun winning-move-strategy (board)
  (let ((pos (find-in-a-row board *opponent2* 2)))
  (cond (pos (list pos "Winning move"))
        (T nil))))

;; Look for a position on the board where they opponent can win the game
(defun defensive-move-strategy (board)
  (let ((pos (find-in-a-row board *opponent1* 2)))
  (cond (pos (list pos "Defensive move"))
        (T nil))))

;; Loop for a position on the board where we can setup a win later
(defun offensive-move-strategy (board)
  nil)

;; Play a single game of tic-tac-toe. This function will prompt the player with
;; a few questions before beginning the game:
;;
;; 1. Does the player want to play the computer?
;;    1. Does he want to play on expert mode?
;;    2. Does the want to make the first move?
;;
;; If playing on 'expert mode' then the *hard-mode-on* global is set to true. If
;; they opt to go first then we let player 1 make his move.
(defun play-one-game ()
  (setf *multiplayer-mode* (not (y-or-n-p "Play against the computer?")))
  (if (not *multiplayer-mode*) (progn
    (setf *hard-mode-on* (y-or-n-p "Would you like to play on expert-mode?"))
    (if (y-or-n-p "Would you like to go first?")
      (opponent-move-p1 (make-board))
    (computer-move (make-board))))
  (opponent-move-p1 (make-board))))
