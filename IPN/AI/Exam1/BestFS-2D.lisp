(load "maze_lib.lisp")

;;; BestF.lisp
;;;   Resuelve el problema de los laberintos usando BEST First Search
;;;
;;;   Representación de los estados:
;;;     Lista con dos elementos: Un valor de aptitud y una lista (x, y) de su posicion
;;;     (aptitud (x y)) 
;;;
;;; Oscar Andres Rosas Hernandez



;;; ==================================================
;;; =========       GLOBAL PARAMETERS        =========
;;; ==================================================
(defparameter  *id*                 -1)                   ;; Cantidad de nodos creados                                          
(defparameter  *open*               ())                   ;; Frontera de busqueda.                                           
(defparameter  *memory-operations*  (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-ancestor*    (make-hash-table))    ;; Memoria de ancestros
(defparameter  *expanded*           0)                    ;; Cuantos estados han sido expandidos
(defparameter  *max-frontier*       0)                    ;; El tamano de la maximo de la frontera 
(defparameter  *closest*            '(9999999999 nil))    ;; Almacena el estado con la mejor solucion 
(defparameter  *current-ancestor*   nil)                  ;; Almacena al ancestro actual (estado)
(defparameter  *aptitude-id*        nil)                  ;; Almacena el nombre de la funcion

(defparameter  *operations*  '((:arriba           0 )
                               (:arriba-derecha   1 )
                               (:derecha          2 )
                               (:abajo-derecha    3 )
                               (:abajo            4 )
                               (:abajo-izquierda  5 )
                               (:izquierda        6 )
                               (:arriba-izquierda 7 )))


;;; ==================================================
;;; =========       INSERT INTO OPEN         =========
;;; ==================================================
(defun insert-in-ordered-list (value state states)
  "Inserta en una lista ordenada en O(n)"
  (let
    ((front (first states)))

    (if (null states)
      (cons state nil)
      (if (<= value (first front))
        (cons state states)
        (cons front (insert-in-ordered-list value state (rest states)))))))

(defun insert-to-open (state)
  "Inserta un estado en la posicion correcta en *open*"
  (setq *open*          (insert-in-ordered-list (first state) state *open*))
  (setq *max-frontier*  (max (length *open*) *max-frontier*)))


;;; ==================================================
;;; =========       APTITUDES FUNCTIONS      =========
;;; ==================================================

(defun Manhattan (coordinates)
  "Te regresa el valor de aptitud de un nodo, mientras mas pequeño mejor"
    (let ( 
      (x1 (first  coordinates))
      (y1 (second coordinates))  
      (x2 (aref *goal* 0))
      (y2 (aref *goal* 1)))

      (+ (abs (- x2 x1)) (abs (- y2 y1)) )))

(defun Euclidean (coordinates)
  "Te regresa el valor de aptitud de un nodo, mientras mas pequeño mejor"
    (let
      ( 
        (x1 (first  coordinates))
        (y1 (second coordinates))  

        (x2 (aref *goal* 0))
        (y2 (aref *goal* 1)))
        (sqrt(+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))))

(defun aptitude (coordinates)
  "Llama a la funcion correcta"
  (case *aptitude-id*
    (0 (Manhattan coordinates))  
    (1 (Euclidean coordinates))  
  ))


;;; ==================================================
;;; =========            MEMORY              =========
;;; ==================================================
(defun  get-hash-point (x y)
  "Te da un ID unico para usarlo como llave en la memoria"
  (+ x (* y (+ 1 (get-maze-rows)))))

(defun  is-first-time-seeing-this-point?  (x y)
  "Predicado. Te regresa si este es la primera vez que veo este estado"
  (null (gethash (get-hash-point x y) *memory-operations*)))

(defun  add-to-memory (state operation)
  "Añade un estado a la memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (val         (get-hash-point x y)))

    (setf (gethash val *memory-operations*) operation)
    (setf (gethash val *memory-ancestor*) *current-ancestor*)))


;;; ==================================================
;;; ======       OLD STATE -> NEW STATE      =========
;;; ==================================================
(defun get-bit-list (door-id)
  "Helper. Te una lista de 1's y 0's para saber si í-esima puerta esta cerrada"
  (loop for i below 4 collect (if (logbitp i door-id) 1 0)))

(defun  valid-position? (x y)
"Predicado. Valida  un estado según las restricciones generales del problema..."
  (let*  
    ((rows (get-maze-rows))  (cols (get-maze-cols)))
    (and (>= x 0) (>= y 0) (< x rows) (< y cols))))

(defun  valid-operator? (operation state)
"Predicado. Valida la aplicación de un operador a un estado, se supone un estado valido" 
  (let*  
    (
      (coordinates  (second state))
      (x  (first  coordinates))
      (y  (second coordinates)))

    (if (not (valid-position? x y)) (return-from valid-operator? nil)))

  (let*  
    (
      (name         (first operation))
      (coordinates  (second state))

      (x  (first  coordinates))
      (y  (second coordinates))

      (x+ (+ x 1))
      (x- (- x 1))
      (y+ (+ y 1))
      (y- (- y 1))

      (doors  (get-bit-list (get-cell-walls x y)))
      (p0     (nth 0 doors))
      (p1     (nth 1 doors))
      (p2     (nth 2 doors))
      (p3     (nth 3 doors))
      
      (rows   (get-maze-rows))
      (cols   (get-maze-cols)))

      (case name
        (:arriba           (and (> x 0)         (eql p0 0)  (is-first-time-seeing-this-point? x- y)))
        (:derecha          (and (< y+ cols)     (eql p1 0)  (is-first-time-seeing-this-point? x y+)))
        (:abajo            (and (< x+ rows)     (eql p2 0)  (is-first-time-seeing-this-point? x+ y)))
        (:izquierda        (and (> y 0)         (eql p3 0)  (is-first-time-seeing-this-point? x y-)))

        (:arriba-derecha   (and (> x 0) (< y+ cols) (is-first-time-seeing-this-point? x- y+)
          (let* (
            (derecha-door (get-bit-list (get-cell-walls x y+)))
            (arriba-door  (get-bit-list (get-cell-walls x- y)))
            (p0-derecha   (nth 0 derecha-door))
            (p1-arriba    (nth 1 arriba-door))
          )
          (or (and (eql 0 p0) (eql 0 p1-arriba)) (and (eql 0 p1) (eql 0 p0-derecha))))))

        (:abajo-derecha    (and (< x+ rows) (< y+ cols) (is-first-time-seeing-this-point? x+ y+)
          (let* (
            (derecha-door (get-bit-list (get-cell-walls x y+)))
            (abajo-door   (get-bit-list (get-cell-walls x+ y)))
            (p1-abajo     (nth 1 abajo-door))
            (p2-derecha   (nth 2 derecha-door))
          )
          (or (and (eql 0 p1) (eql 0 p2-derecha)) (and (eql 0 p2) (eql 0 p1-abajo))))))

        (:abajo-izquierda  (and (< x+ rows) (> y 0) (is-first-time-seeing-this-point? x+ y-)
          (let* (
            (izquierda-door (get-bit-list (get-cell-walls x y-)))
            (abajo-door     (get-bit-list (get-cell-walls x+ y)))
            (p2-izquierda   (nth 2 izquierda-door))
            (p3-abajo       (nth 3 abajo-door))
          )
          (or (and (eql 0 p2) (eql 0 p3-abajo)) (and (eql 0 p3) (eql 0 p2-izquierda))))))

        (:arriba-izquierda (and (> x 0) (> y 0) (is-first-time-seeing-this-point? x- y-)
          (let* (
            (arriba-door    (get-bit-list (get-cell-walls x- y)))
            (izquierda-door (get-bit-list (get-cell-walls x y-)))
            (p3-arriba      (nth 3 arriba-door))
            (p0-izquierda   (nth 0 izquierda-door))
          )
          (or (and (eql 0 p3) (eql 0 p0-izquierda)) (and (eql 0 p0) (eql 0 p3-arriba))))))
      )))


(defun  apply-operator (operation state) 
"Obtiene el descendiente de un estado al aplicarle una operacion SIN VALIDACIONES"
    (let*  
      (
        (coordinates (second state))
        (x  (first  coordinates))
        (y  (second coordinates))
        (x+ (+ x 1))
        (x- (- x 1))
        (y+ (+ y 1))
        (y- (- y 1))

        (name (first operation))

        (new-coordinates
          (case name
            (:arriba           (list x- y ))
            (:arriba-derecha   (list x- y+))
            (:derecha          (list x  y+))
            (:abajo-derecha    (list x+ y+))
            (:abajo            (list x+ y ))
            (:abajo-izquierda  (list x+ y-))
            (:izquierda        (list x  y-))
            (:arriba-izquierda (list x- y-)))))

      (list (aptitude new-coordinates) new-coordinates)))

;;; ==================================================
;;; ======           EXPAND STATE            =========
;;; ==================================================
(defun update-closest-state (state)
"Get better node"
  (if
    (< (first state) (first *closest*))
    (setq *closest* state)))

(defun expand (state)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *operations*"
  (setq *current-ancestor* state)
  (let 
    ((new-state nil))

    (incf *expanded*)
    (dolist (operation *operations*)
      (cond 
        ((valid-operator? operation state)

          (incf  *id*)
          (setq  new-state      (apply-operator operation state))
          (update-closest-state new-state)
          (add-to-memory        new-state (second operation))
          (insert-to-open       new-state)                )))))



;;; ==================================================
;;; ======             SOLUTION              =========
;;; ==================================================
(defun extract-solution (state)
"Rastrea en *memory* todos los descendientes de state hasta llegar al estado inicial"
  (let
    (
      (current    state)
      (operation  nil)
      (ansestor   nil)
      (value      nil))

      (loop  while  (not (null current)) do
        (setq value     (get-hash-point (first (second current)) (second (second current)) ))
        (setq operation (gethash value *memory-operations*))
        (setq ansestor  (gethash value *memory-ancestor*))
        (setq current   ansestor)

        (push operation *solution*))

      (setq *solution* (rest *solution*))))

(defun  display-solution ()
"Despliega la solución en forma conveniente y numerando los pasos"
  (format t "~%La solucion es: ~A ~%~%" *solution*)

  (format t "1) Solución con ~A pasos (Longitud de la solución)~%" (length *solution*))
  (format t "2) ~A nodos creados ~%" *id*)
  (format t "3) ~A nodos expandidos ~%" *expanded*)
  (format t "4) Longitud máxima de la Frontera de búsqueda: ~A~%" *max-frontier*))

(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
  (setq  *id*               -1)                         
  (setq  *open*             ())                       
  (setq  *memory-operations*      (make-hash-table))   
  (setq  *memory-ancestor*  (make-hash-table))   
  (setq  *expanded*         0)            
  (setq  *max-frontier*     0)
  (setq  *closest*          '(9999999999 nil))
  (setq  *current-ancestor* nil))

(defun get-start ()
  "Te regresa el estado inicial"
    (let*
      ( 
        (x (aref  *start* 0))
        (y (aref  *start* 1))
        (coordinate (list x y)))
      (list (aptitude coordinate) coordinate)))

(defun get-goal ()
  "Te regresa el estado inicial"
    (let*
      ( 
        (x (aref  *goal* 0))
        (y (aref  *goal* 1))
        (coordinate (list x y)))
      (list (aptitude coordinate) coordinate)))


;;; ==================================================
;;; ======             BESTFS                =========
;;; ==================================================
(defun  BestFSearch ()
"Realiza una búsqueda best First, desde un estado inicial hasta un estado meta"
  (reset-all)
  (let
    (
      (start  (get-start))
      (goal    (get-goal))

      (current     nil)
      (sucesores   nil)
      (goal-found  nil))

      (insert-to-open start)
      (add-to-memory  start -1)

      (time 
        (loop until (or goal-found (null *open*)) do
          (setq current (pop *open*))
          
          (cond    
            ((equal goal current)
              (setq  goal-found T)
              (format t "Éxito. Meta encontrada ~%")
            )

            (t
              (expand current)
              (if (null *open*) 
                (progn 
                  (format t "Lo intenté.%")
                  (setq current *closest*)
                ))
            ))
        )
      )
      
      (extract-solution current)
      (display-solution))
          
  (print (get-maze-data))
  (print *start*)
  (print *goal*))


(defun BestFSearch-Manhattan ()
  (setq *aptitude-id* 0)
  (BestFSearch))

(defun BestFSearch-Euclidean ()
  (setq *aptitude-id* 1)
  (BestFSearch))

(add-algorithm 'BestFSearch-Manhattan)
(add-algorithm 'BestFSearch-Euclidean)
(start-maze)