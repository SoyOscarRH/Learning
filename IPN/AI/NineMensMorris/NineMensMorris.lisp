;;; NineMensMorris.lisp
;;;
;;; Oscar Andres Rosas Hernandez

;;;          a - - b - - c 
;;;          | d - e - f | 
;;;          | | g h i | | 
;;;          j k l + m n o 
;;;          | | p q r | | 
;;;          | s - t - u | 
;;;          v - - w - - x

;;; ==================================================
;;; =========      CREATE FUNCTIONS          =========
;;; ==================================================
(defun create-keys ()
  "Create a hash table, from a char-code to a coordinate"
  (let
    ((keys (make-hash-table)))

    (defun set-hash (code-number point)
      (setf (gethash (code-char code-number) keys) point))

    (set-hash 097 '(0 0)) (set-hash 098 '(0 3)) (set-hash 099 '(0 6))
    (set-hash 100 '(1 1)) (set-hash 101 '(1 3)) (set-hash 102 '(1 5))
    (set-hash 103 '(2 2)) (set-hash 104 '(2 3)) (set-hash 105 '(2 4))

    (set-hash 106 '(3 0)) (set-hash 107 '(3 1)) (set-hash 108 '(3 2))
    (set-hash 109 '(3 4)) (set-hash 110 '(3 5)) (set-hash 111 '(3 6))

    (set-hash 112 '(4 2)) (set-hash 113 '(4 3)) (set-hash 114 '(4 4))
    (set-hash 115 '(5 1)) (set-hash 116 '(5 3)) (set-hash 117 '(5 5))
    (set-hash 118 '(6 0)) (set-hash 119 '(6 3)) (set-hash 120 '(6 6))

    keys)
)

(defun create-board ()
  "Create a board, a 7x7 ascii table"
  
  (defun chr (number) (code-char number))

  (make-array 
    '(7 7) :initial-contents
    (list
      (list nil #\- #\- nil #\- #\- nil)
      (list #\| nil #\- nil #\- nil #\|)
      (list #\| #\| nil nil nil #\| #\|)
      (list nil nil nil #\+ nil nil nil)
      (list #\| #\| nil nil nil #\| #\|)
      (list #\| nil #\- nil #\- nil #\|)
      (list nil #\- #\- nil #\- #\- nil)
    ))
)

(defparameter 
  *moves* 
  '(
    (0 0 #\a) (0 3 #\b) (0 6 #\c)
    (1 1 #\d) (1 3 #\e) (1 5 #\f)
    (2 2 #\g) (2 3 #\h) (2 4 #\i)

    (3 0 #\j) (3 1 #\k) (3 2 #\l)
    (3 4 #\m) (3 5 #\n) (3 6 #\o)
    
    (4 2 #\p) (4 3 #\q) (4 4 #\r)
    (5 1 #\s) (5 3 #\t) (5 5 #\u)
    (6 0 #\v) (6 3 #\w) (6 6 #\x)
  )
)

(defparameter *me*    #\@)
(defparameter *other* #\*)

;;; ==================================================
;;; ==========     HELPERS FUNCTIONS       ===========
;;; ==================================================
(defun copy-array (array &key (element-type (array-element-type array))
                              (fill-pointer (and (array-has-fill-pointer-p array)
                                                 (fill-pointer array)))
                              (adjustable (adjustable-array-p array)))
    "Algoritmo de la biblioteca Alexandria, no es de mi autoria (https://common-lisp.net/project/alexandria/), 
    necesaria para mi implementación"
    (let* ((dimensions (array-dimensions array))
          (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array)
)

(defun print-board (board)
  (let
    (
      (new-board (copy-array board))
      (code  97))

      (loop for move in *moves* do
        (let*
          ((x (first move)) (y (second move)))
          (if (null (aref new-board x y))
            (setf (aref new-board x y) (code-char code)))
          
          (incf code)))

      (loop for i from 0 to 6 do
        (loop for j from 0 to 6 do
          (cond 
            ((eql (aref new-board i j) #\@)
              (format T "~c[91m~c~c[0m " #\ESC (aref new-board i j) #\ESC))
            ((eql (aref new-board i j) #\*)
              (format T "~c[32m~c~c[0m " #\ESC (aref new-board i j) #\ESC))
            (t
              (format T "~a " (aref new-board i j)))
          ))
          
        (format T "~%")))
)

(defun change (board keys letter player)
  (let*
    (
      (point (gethash letter keys))
      (x (first point))
      (y (second point)))

    (setf (aref board x y) player))
)

;;; ==================================================
;;; ==========        EVALUATE             ===========
;;; ==================================================
(defun evaluate-for (board player other)
  "Evalua un estado (tablero) para un jugador bajo la siguiente heuristica:
    - 1 punto si no hay una marca tuya en una linea
    - 10 puntos si hay 1 marca tuya en una linea
    - 100 puntos si hay 2 marcas tuyas en una linea 
    - 1000 puntos si hay 3 marcas tuyas en una linea
  "
  (let 
    (
      (local-row    0)
      (local-column 0)
      (result 0))

    (loop for x from 0 to 6 do
      (setq local-row    0)
      (setq local-column 0)
      (loop for y from 0 to 6 do
        (if (eql (aref board x y) player) 
          (incf local-row))
        (if (eql (aref board y x) player) 
          (incf local-column))
      )
      
      (incf result (expt 10 local-row))
      (incf result (expt 10 local-column))
    )

    result)
)

(defun evaluate (board)
  "My points vs yours"
  (- (evaluate-for board *me* *other*) (evaluate-for board *other* *me*))
)

;;; ==================================================
;;; ==========           MOVES             ===========
;;; ==================================================
(defun get-moves (board player)
  "Dame una lista de (tablero jugada) posibles dado un tablero, el orden de esta, maximiza podas"
  (let
    (
      (new-board nil)
      (moves nil))

      (loop for move in *moves* do
        (let* 
          ((x (first move)) (y (second move)) (letter (third move)))

          (cond ((null (aref board x y))
              (setq new-board (copy-array board))
              (setf (aref new-board x y) player)
              (push (list new-board letter) moves)
          ))
      ))

      moves
  )
)


;;; ==================================================
;;; ==========           MIN MAX           ===========
;;; ==================================================
(defun minimax-alphabeta (board depth alpha beta maximizing-player)
  "Regresa la mejor jugada y la evaluacion a realizar para O dado un tablero (usa poda alpha beta)"
  (if (zerop depth)
    (return-from minimax-alphabeta (list (evaluate board) nil)))

  (if maximizing-player
    (let
      (
        (current-board nil)
        (current-move nil)
        (new-depth (- depth 1))
        (max-evaluation most-negative-fixnum)
        (evaluation nil)
        (best-move nil))

        (loop for board-move in (get-moves board *me*) do
          (setq current-board (first board-move))
          (setq current-move (second board-move))

          (setq evaluation (first (minimax-alphabeta current-board new-depth alpha beta nil)))

          (cond ((> evaluation max-evaluation)
            (setq max-evaluation evaluation)
            (setq best-move current-move)))

          (setq alpha (max alpha evaluation))

          (if (<= beta alpha) (return-from minimax-alphabeta (list max-evaluation best-move))))

        (return-from minimax-alphabeta (list max-evaluation best-move)))

    (let
      (
        (current-board nil)
        (current-move nil)
        (new-depth (- depth 1))
        (min-evaluation most-positive-fixnum)
        (evaluation nil)
        (best-move nil))

        (loop for board-move in (get-moves board *other*) do
          (setq current-board (first board-move))
          (setq current-move (second board-move))

          (setq evaluation (first (minimax-alphabeta current-board new-depth alpha beta T)))

          (cond ((< evaluation min-evaluation)
            (setq min-evaluation evaluation)
            (setq best-move current-move)))

          (setq beta (min beta evaluation))

          (if (<= beta alpha) (return-from minimax-alphabeta (list min-evaluation best-move))))

        (return-from minimax-alphabeta (list min-evaluation best-move)))
  )
)

;;; ==================================================
;;; ==========        MAIN                 ===========
;;; ==================================================
(let
  (
    (board (create-board))
    (keys (create-keys))
    (player-me 9)
    (player-other 9)
    (move  #\a)
  )

  (change board keys #\a *other*)
  (change board keys #\x *me*)

  (format t "Board: ~%")
  (print-board board)

  (loop while (or (> player-me 2) (> player-other 2)) do 
    (format t "~%Select a letter to place a move: ~%")
    (setq move (read-char))
    (read-char)

    (change board keys move *other*)
    (decf player-other)
    (print-board board)

    (setq move (second 
      (minimax-alphabeta board 4 most-negative-fixnum most-positive-fixnum T)
    ))

    (format t "~%Moving the ~a: ~%" move)

    (change board keys move *me*)
    (decf player-me)
    (print-board board)
  )
)


