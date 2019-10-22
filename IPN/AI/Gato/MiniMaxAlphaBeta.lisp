;;; MiniMaxAlphaBeta.lisp
;;;   Resuelve el problema de gato usando minimax
;;;
;;; Oscar Andres Rosas Hernandez


;;; ==================================================
;;; =========      FUNCIONES AUXILIARES      =========
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
    new-array))


(defun create-board (list-board)
  "De una lista a una matrix"
  (make-array '(4 4) :initial-contents list-board))


;;; ==================================================
;;; =========      FUNCIONES HEURISTICAS     =========
;;; ==================================================

(defun evaluate-for (board player)
  "Evalua un estado (tablero) para un jugador bajo la siguiente heuristica:
    - 1 punto si no hay una marca tuya en una linea o diagonal
    - 10 puntos si hay 1 marca tuya en una linea o diagonal
    - 100 puntos si hay 2 marcas tuyas en una linea o diagonal
    - 1000 puntos si hay 3 marcas tuyas en una linea o diagonal
    - 10000 puntos si hay 4 marcas tuyas en una linea o diagonal
  "
  (let 
    (
      (local-count 0)
      (result 0))

    ;;; checa filas
    (loop for row from 0 to 3 do
      (setq local-count 0)
      (loop for i from 0 to 3 do
        (if (eql (aref board row i) player) 
          (incf local-count)))
          
      (setq result (+ result (expt 10 local-count))))

    ;;; checa columnas
    (loop for column from 0 to 3 do
      (setq local-count 0)
      (loop for i from 0 to 3 do
        (if (eql (aref board i column) player) 
          (incf local-count)))
          
      (setq result (+ result (expt 10 local-count))))

    ;;; checa diagonal
    (setq local-count 0)
    (loop for i from 0 to 3 do
      (if (eql (aref board i i) player)
          (incf local-count)))

    (setq result (+ result (expt 10 local-count)))

    ;;; checa diagonal
    (setq local-count 0)
    (loop for i from 0 to 3 do
      (if (eql (aref board i (- 3 i)) player)
          (incf local-count)))

    (setq result (+ result (expt 10 local-count)))

    result
  )
)


(defun evaluate (board)
  "Mis puntos menos los tuyos"
  (- (evaluate-for board 'O) (evaluate-for board 'X))
)


(defun game-over-player (board player)
  "Regresa que jugador gano o nil si aun no hay"
  (let 
    ((won nil) (result 0))

    ;;; checa filas
    (loop for row from 0 to 3 do
      (setq won T)
      (loop for i from 0 to 3 do
        (if (not (eql (aref board row i) player))
          (setq won nil)))
          
      (if won (return-from game-over-player T)))

    ;;; checa columnas
    (loop for column from 0 to 3 do
      (setq won T)
      (loop for i from 0 to 3 do
        (if (not (eql (aref board i column) player))
          (setq won nil)))

      (if won (return-from game-over-player T)))


    ;;; checa diagonales
    (setq won T)
    (loop for i from 0 to 3 do
      (if (not (eql (aref board i i) player))
          (setq won nil))
    )
    (if won (return-from game-over-player T))

    ;;; checa diagonales
    (setq won T)
    (loop for i from 0 to 3 do
      (if (not (eql (aref board i (- 3 i)) player))
          (setq won nil))
    )
    (if won (return-from game-over-player T))

    nil
  )
)

(defun game-over (board)
  "No importa quien gano, solo que alguien gano"
  (or (game-over-player board 'O) (game-over-player board 'X))
)


(defparameter 
  *moves* 
  '(
    ;; Primero probemos los movimientos normales
    (0 1   2)
    (0 2   3)
    (3 1  14)
    (3 2  15)
    (1 0   5)
    (1 3   8)
    (2 0   9)
    (2 3  12)

    ;; Lo siguiente mejor es probar las esquinas
    (0 0   1)
    (0 3   4)
    (3 0  13)
    (3 3  16)

    ;; Lo mejor es probar primero los centros
    (1 1   6)
    (1 2   7)
    (2 1  10)
    (2 2  11)
  )
) 
(defun get-moves (board is-O-playing)
  "Dame una lista de (tablero jugada) posibles dado un tablero, el orden de esta, maximiza podas"
  (let
    (
      (new-board nil)
      (mark (if is-O-playing 'O 'X))
      (moves nil))

      (loop for move in *moves* do
        (let 
          (
            (i   (first  move))
            (j   (second move))
            (num (third  move)))

          (cond ((null (aref board i j))
              (setq new-board (copy-array board))
              (setf (aref new-board i j) mark)
              (push (list new-board num) moves)
          ))
      ))

      moves
  )
)


(defun minimax-alphabeta (board depth alpha beta maximizing-player)
  "Regresa la mejor jugada y la evaluacion a realizar para O dado un tablero (usa poda alpha beta)"
  (if (or (zerop depth) (game-over board)) 
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

        (loop for board-move in (get-moves board maximizing-player) do
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

        (loop for board-move in (get-moves board maximizing-player) do
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

(defun tictactoe (list-board)
  "Control del programa, es solo un wrapper sobre minmax"
  (setq 
    *output* 
    (second 
      (minimax-alphabeta
        (create-board list-board) 2 most-negative-fixnum most-positive-fixnum T
      )
    )
  )
)
