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
  "Create a hash table with a letter to a coordinate"
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
      (list (chr 097)  #\-       #\-       (chr 098) #\-       #\-       (chr 099))
      (list #\|        (chr 100) #\-       (chr 101) #\-       (chr 102) #\|)
      (list #\|        #\|       (chr 103) (chr 104) (chr 105) #\|       #\|)
      (list (chr 106)  (chr 107) (chr 108) #\+       (chr 109) (chr 110) (chr 111))
      (list #\|        #\|       (chr 112) (chr 113) (chr 114) #\|       #\|)
      (list #\|        (chr 115) #\-       (chr 116) #\-       (chr 117) #\|)
      (list (chr 118)  #\-       #\-       (chr 119) #\-       #\-       (chr 120))
    ))
)

;;; ==================================================
;;; ==========     HELPERS FUNCTIONS       ===========
;;; ==================================================
(defun print-board (board)
  (loop for i from 0 to 6 do
    (loop for j from 0 to 6 do
      (format T "~a " (aref board i j)))
    (format T "~%"))
)


;;; ==================================================
;;; ==========        EVALUATE             ===========
;;; ==================================================
(defun evaluate-for (board player)
  "Evalua un estado (tablero) para un jugador bajo la siguiente heuristica:
    - 1 punto si no hay una marca tuya en una linea o diagonal
    - 10 puntos si hay 1 marca tuya en una linea o diagonal
    - 100 puntos si hay 2 marcas tuyas en una linea o diagonal
    - 1000 puntos si hay 3 marcas tuyas en una linea o diagonal
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
  "My points vs yours"
  (- (evaluate-for board '*) (evaluate-for board '&))
)




(defun chr (number)
    (code-char number))

(let
  (
    (board (create-board))
    (keys  (create-keys)))

    (setf (aref board 0 0) #\*)
    (print-board board)

)
