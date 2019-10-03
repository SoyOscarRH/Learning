(load "maze_lib.lisp")

;;; BestF.lisp
;;;   Resuelve el problema de los laberintos usando A*
;;;
;;;   Representación de los estados:
;;;     Lista con dos elementos: Un valor de aptitud y una lista (x, y) de su posicion
;;;     (aptitud (x y)) 
;;;
;;; Oscar Andres Rosas Hernandez


;;; ==================================================
;;; =========       GLOBAL PARAMETERS        =========
;;; ==================================================
(defparameter  *id*                -1)                   ;; Cantidad de nodos creados                                          
(defparameter  *open*              ())                   ;; Frontera de busqueda.                                           
(defparameter  *memory-open*       (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-operations* (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-ancestor*   (make-hash-table))    ;; Memoria de ancestros
(defparameter  *memory-distance*   (make-hash-table))    ;; Memoria de la distancia a ese nodo
(defparameter  *expanded*          0)                    ;; Cuantos estados han sido expandidos
(defparameter  *max-frontier*      0)                    ;; El tamano de la maximo de la frontera 
(defparameter  *closest*           '(9999999999 nil))    ;; Almacena el estado con la mejor solucion 
(defparameter  *current-ancestor*  nil)                  ;; Almacena al ancestro actual (estado)
(defparameter  *aptitude-id*       nil)                  ;; Almacena el nombre de la funcion


(defparameter  *operations*  '( (:arriba           0)
                                (:derecha          2)
                                (:abajo            4)
                                (:izquierda        6)))


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


(defun delete-from-ordered-list (coordinates states)
  "Elimina en una lista ordenada en O(n)"
  (if (null states) (return-from delete-from-ordered-list nil))
  (let
    ( 
      (front (first states))
      (end    (rest states)))
    
      (if (equal coordinates (second front))
        end
        (cons front (delete-from-ordered-list coordinates end)))))

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

(defun get-cost-and-aptitude (coordinates)
"Te regresa el valor de aptitud de un nodo, mientras mas pequeño mejor"
  (let
    ( 
      (aptitude-value (aptitude coordinates))
      (distance-value (get-distance (list 0 coordinates))))

      (+ aptitude-value distance-value)))



;;; ==================================================
;;; =========            MEMORY              =========
;;; ==================================================
(defun  get-hash-point (x y z)
  "Te da un ID unico para usarlo como llave en la memoria"
  (+(* 2 (+ x (* y (+ 1 (get-maze-rows))))) z))

(defun  is-first-time-seeing-this-point?  (x y z)
  "Predicado. Te regresa si este es la primera vez que veo este estado"
  (null (gethash (get-hash-point x y z) *memory-operations*)))

(defun  add-to-memory (state operation)
  "Añade un estado a la memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (z           (third coordinates))
      (val         (get-hash-point x y z)))

    (setf (gethash val *memory-operations*) operation)
    (setf (gethash val *memory-ancestor*) *current-ancestor*)))


;;; ==================================================
;;; =========     DISTANCE (NODE'S DEPTH)     ========
;;; ==================================================
(defun  set-distance  (state distance)
  "Anadelo a memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (z           (third  coordinates))
      (val         (get-hash-point x y z)))

    (setf (gethash val *memory-distance*) distance)))


(defun  get-distance  (state)
  "Obten su valor en memoria, si no esta dale un 0 (origen)"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (z           (third  coordinates))
      (val         (get-hash-point x y z)) 
      (distance    (gethash val *memory-distance*))
    )

    (if (null distance) 0 distance)))



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

(defun  apply-operator (operation state)
"Predicado. Valida la aplicación de un operador a un estado, si no es valido regresa nil"
  (let*  
      (
        (name (first operation))
        (coordinates1 (second state))

        (x1 (first   coordinates1))
        (y1 (second  coordinates1))
        (z1 (third   coordinates1))

        (x x1)
        (y y1)

        (x+ (+ x 1) )
        (x- (- x 1) )
        (y+ (+ y 1) )
        (y- (- y 1) )

        (coordinates2
          (case name
            (:arriba           (list x- y ))
            (:derecha          (list x  y+))
            (:abajo            (list x+ y ))
            (:izquierda        (list x  y-))
          )
        )

        (x2 (first   coordinates2))
        (y2 (second  coordinates2))
        (z2 0)
      )

      ; Si ni parece valido 
      (if (not (and (valid-position? x1 y1) (valid-position? x2 y2)))
        (return-from apply-operator nil)   )

      (let*
        (
          (door  (get-cell-walls x1 y1))
          (door2 (get-cell-walls x2 y2))

          (door-data (get-bit-list door))
          
          (rows (get-maze-rows))        
          (cols (get-maze-cols))

          (vertical  (eql (rem (second operation) 4) 0))
        )
          
          (if (and (eql door 16) (eql z1 1) vertical)
            (return-from apply-operator nil)
          )

          (if (and (eql door 16) (eql z1 0) (not vertical))
            (return-from apply-operator nil)
          )

          (if (and (eql door 17) (eql z1 0) vertical)
            (return-from apply-operator nil)
          )

          (if (and (eql door 17) (eql z1 1) (not vertical))
            (return-from apply-operator nil)
          )


          (if (> door 15)
            (setq door (if vertical 10 5))
          )


          (if 
            (not (case name
              (:arriba     (eql (nth 0 door-data ) 0))
              (:derecha    (eql (nth 1 door-data ) 0))
              (:abajo      (eql (nth 2 door-data ) 0))
              (:izquierda  (eql (nth 3 door-data ) 0))
            ))
            (return-from apply-operator nil)
          )

          (if
            (and
              (> door2 15)
              (or 
                (and (eql door2 16) (not vertical))
                (and (eql door2 17) vertical)
              )
            )
            (setq z2 1)
          )


          (if (is-first-time-seeing-this-point? x2 y2 z2)
          (list (get-cost-and-aptitude (list x2 y2 z2) ) (list x2 y2 z2)) nil)

      )))


;;; ==================================================
;;; ======           EXPAND STATE            =========
;;; ==================================================
(defun update-closest-state (state)
"Get better node"
  (if
    (< (first state) (first *closest*))
    (setq *closest* state)))

(defun get-hash-coordinate (coordinate)
"Get better node"
  (let*
    (
      (x (first  coordinate))
      (y (second coordinate))
      (z (second coordinate)))

      (get-hash-point x y z)))

(defun expand (state)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *operations*"
  (setq  *current-ancestor* state)
  (let*
    (
      (val       (get-hash-coordinate (second state)))
      (new-state nil)
      (pre-value nil))

    (incf  *expanded*)
    (setf (gethash val *memory-open*) Nil)  ; Ya no estoy en open

    (dolist  (operation  *operations*)
      (setq  new-state  (apply-operator operation state))

      (cond 
        ((not (null new-state))

          (incf  *id*)
          (update-closest-state new-state)

          (setq pre-value (gethash (get-hash-coordinate (second new-state)) *memory-open*))

          (set-distance 
            new-state
            (+ 1 (get-distance *current-ancestor*)))

          (add-to-memory new-state (second operation))

          (if 
            (and (not (null pre-value)) (< pre-value (first new-state)))
            (delete-from-open (second new-state)))

          (insert-to-open new-state))))))


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
        (setq value     (get-hash-point (first (second current)) (second (second current)) (third (second current)) ))
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
        (z 0)
        (coordinate (list x y z)))
      (list (aptitude coordinate) coordinate)))

(defun get-goal ()
  "Te regresa el estado inicial"
    (let*
      ( 
        (x (aref  *goal* 0))
        (y (aref  *goal* 1))
        (z 0)
        (coordinate (list x y z)))
      (list (aptitude coordinate) coordinate)))


(defun  AStar ()
"Realiza una búsqueda A*, desde un estado inicial hasta un estado meta"
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
			     


(defun AStar-Manhattan ()
  (setq *aptitude-id* 0)
  (AStar))

(defun AStar-Euclidean ()
  (setq *aptitude-id* 1)
  (AStar))


(add-algorithm 'AStar-Manhattan)
(add-algorithm 'AStar-Euclidean)
(start-maze)






