;Biblioteca maze-lib.lisp - Versión para descargar.

;Variables propias de esta biblioteca. Favor de no usarse o modificarse dentro de su código.
(defvar *algorithms-list* nil)
(defvar *goal*)
(defvar *start*)
(defvar *solution* nil)
(defvar *exec_time* 100000)
(defvar *num_algorithm* 0)
(defvar *num_laberinto* 0)
(defvar *maze*)

(defclass maze nil 
  ((data
     :initarg :data
     :initform (make-array '(1 1)))
   (start_position
     :initarg :start_position
     :initform #(0 0))
   (goal_position
     :initarg :goal_position
     :initform #(1 1))
   (dimensions
     :initarg :dimensions
     :initform '(5 5))))

(setq *maze* 
  (make-instance 'maze 
                 :data #2A((13 1 3 12 3) (9 6 12 3 10) (12 5 3 10 10) (3 9 6 10 10) (12 6 13 4 6))
                 :start_position #(0 3)
                 :goal_position #(3 0)))

(setq *start* (slot-value *maze* 'start_position))
(setq *goal* (slot-value *maze* 'goal_position))

(defmacro add-algorithm (algoritmo)
  ;Añade un algoritmo a ejecutar.
  `(setq *algorithms-list* (append *algorithms-list* (list ,algoritmo))))

(defun get-maze-data ()
  ;Obtiene los datos del laberinto
  (slot-value *maze* 'data))

(defun get-cell-walls (x y)
 ;Regresa las paredes de una celda del laberinto.
  (let ((maze_size (array-dimensions (get-maze-data))))
    (cond
      ((and (>= x 0) (< x (nth 0 maze_size)) (>= y 0) (< y (nth 1 maze_size))) 
       (aref (get-maze-data) x y))
      (t (error "Coordenadas fuera de las dimensiones del laberinto.")))))

(defun draw-cell-walls (x y)
  ;Dibuja las paredes del laberinto, solo como referencia.
  (let ((paredes (get-cell-walls x y)))
    (case paredes
      (0 (format t "~%~%~%"))
      (1 (format t "────~%~%"))
      (2 (format t "   │~%   │~%"))
      (3 (format t "───┐~%   │~%"))
      (4 (format t "~%~%────"))
      (5 (format t "────~%~%────"))
      (6 (format t "   │~%   │~%───┘"))
      (7 (format t "───┐~%   │~%───┘"))
      (8 (format t "│~%│~%"))
      (9 (format t "┌───~%│~%"))
      (10 (format t "│  │~%│  │~%"))
      (11 (format t "┌──┐~%│  │~%"))
      (12 (format t "│~%│~%└───"))
      (13 (format t "┌───~%│~%└───"))
      (14 (format t "│  │~%│  │~%└──┘"))
      (15 (format t "┌──┐~%│  │~%└──┘")))))

(defun get-maze-rows ()
;Regresa las filas del laberinto.
  (first (slot-value *maze* 'dimensions)))

(defun get-maze-cols ()
;Regresa las columnas del laberinto
  (second (slot-value *maze* 'dimensions)))

(defun start-maze ()
  ;Función para procesar la línea de comandos.
  (loop for k from 1 below (length *posix-argv*) do
        (eval (read-from-string (nth k *posix-argv*)))))

