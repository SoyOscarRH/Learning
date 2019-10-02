(load "maze_lib.lisp")

;;; BestF.lisp
;;;   Resuelve el problema de los laberintos usando best first search
;;;
;;;   Representación de los estados:
;;;     Lista con dos elementos: Un valor de aptitud y una lista (x, y) de su posicion
;;;     (aptitud (x y)) 
;;;
;;; Oscar Andres Rosas Hernandez

(defparameter  *id*         -1)                   ;; Cantidad de nodos creados                                          
(defparameter  *open*       ())                   ;; Frontera de busqueda.                                           
(defparameter  *memory-op*  (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-an*  (make-hash-table))    ;; Memoria de ancestros
(defparameter  *expanded*         0)              ;; Almacena cuantos estados han sido expandidos
(defparameter  *max-frontier*     0)              ;; Almacena el tamano de la maximo de la frontera 
(defparameter  *closest*     '(9999999999 nil))   ;; Almacena el la mejor solucion 
(defparameter  *current-ancestor* nil)            ;; referencia al ancestro actual

(defparameter  *ops*  '( (:arriba           0 )
                         (:arriba-derecha   1 )
                         (:derecha          2 )
                         (:abajo-derecha    3 )
                         (:abajo            4 )
                         (:abajo-izquierda  5 )
                         (:izquierda        6 )
                         (:arriba-izquierda 7 )  ) )


(defun push-to-ordered-list (value state states)
  "Inserta en una lista ordenada en O(n)"
  (let
    ( 
      (front (first states))
      (end    (rest states)) )

    (if (null states)
      (cons state nil)
      (if (<= value (first front))
          (cons state states)
          (cons front (push-to-ordered-list value state end) ))) ) )

(defun insert-to-open (state)
  "Recibe un estado, una operacion con el que llegamos ahi y lo inserta segun aptitud"
     (let 
        ((current-frontier   (+ 1 (length *open*))) )
          (setq *open*          (push-to-ordered-list (first state) state *open*))
          (setq *max-frontier*  (max current-frontier *max-frontier*))   ) )

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de frontera de busqueda *open*"
  (pop  *open*) )

(defun aptitude (coordinates)
  "Te regresa el valor de aptitud de un nodo, mientras mas pequeño mejor"
    (let
      ( 
        (x1 (first  coordinates))
        (y1 (second coordinates))  

        (x2 (aref *goal* 0))
        (y2 (aref *goal* 1))  
      )
        
        (+ (abs (- x1 x2)) (abs (- y1 y2)) )
    ) )

;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  get-hash-inline  (x y)
  "Te da un ID unico para usarlo como llave en la memoria"
  (+ x (* y (+ 1 (get-maze-rows)))))

(defun  not-remember-state-inline?  (x y)
  "Ya he visto esto antes?"
  (null (gethash (get-hash-inline x y) *memory-op*))  )

(defun  add-to-memory  (state op)
  "Anadelo a memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (val         (get-hash-inline x y)) )

    (setf (gethash val *memory-op*) op)
    (setf (gethash val *memory-an*) *current-ancestor*)

  ) )


;;;=======================================================================================
;;  VALID-OPERATOR [op, state]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun 
;;                    los recursos en la orilla de la barca
;;;=======================================================================================
(defun get-bit-list (cell-id)
  "Te una lista para saber si la puerta esta cerrada"
    (loop for i below 4 collect (if (logbitp i cell-id) 1 0)) )


(defun  valid-operator? (op state)
"Predicado. Valida la aplicación de un operador a un estado, se supone el estado valido"  
  (let*  
      (
        (coordinates (second state))
        (x (first  coordinates))
        (y (second coordinates))
        
        (x+ (+ x 1) )
        (x- (- x 1) )
        (y+ (+ y 1) )
        (y- (- y 1) )

        (name (first op))

        (doors 
          (if (valid-position? x y)
            (get-bit-list (get-cell-walls x y))
            nil )         )
        (p0 (nth 0 doors))
        (p1 (nth 1 doors))
        (p2 (nth 2 doors))
        (p3 (nth 3 doors))
        
        (rows (get-maze-rows))        
        (cols (get-maze-cols))   )

        (case name
          (:arriba           (and (> x 0)                 (eql p0 0) (not-remember-state-inline? x- y)))
          (:derecha          (and (< y+ cols)             (eql p1 0) (not-remember-state-inline? x y+)))
          (:abajo            (and (< x+ rows)             (eql p2 0) (not-remember-state-inline? x+ y)))
          (:izquierda        (and (> y 0)                 (eql p3 0) (not-remember-state-inline? x y-)))

          (:arriba-derecha   (and (> x 0) (< y+ cols) (not-remember-state-inline? x- y+)
            (let* (
              (derecha-door (get-bit-list (get-cell-walls x y+)))
              (arriba-door  (get-bit-list (get-cell-walls x- y)))
              (p0-derecha (nth 0 derecha-door))
              (p1-arriba  (nth 1 arriba-door))
            )
            (or (and (eql 0 p0) (eql 0 p1-arriba)) (and (eql 0 p1) (eql 0 p0-derecha))))))

          (:abajo-derecha    (and (< x+ rows) (< y+ cols) (not-remember-state-inline? x+ y+)
            (let* (
              (derecha-door (get-bit-list (get-cell-walls x y+)))
              (abajo-door   (get-bit-list (get-cell-walls x+ y)))
              (p1-abajo   (nth 1 abajo-door))
              (p2-derecha (nth 2 derecha-door))
            )
            (or (and (eql 0 p1) (eql 0 p2-derecha)) (and (eql 0 p2) (eql 0 p1-abajo))))))

          (:abajo-izquierda  (and (< x+ rows) (> y 0) (not-remember-state-inline? x+ y-)
            (let* (
              (izquierda-door (get-bit-list (get-cell-walls x y-)))
              (abajo-door     (get-bit-list (get-cell-walls x+ y)))
              (p2-izquierda (nth 2 izquierda-door))
              (p3-abajo     (nth 3 abajo-door))
            )
            (or (and (eql 0 p2) (eql 0 p3-abajo)) (and (eql 0 p3) (eql 0 p2-izquierda))))))

          (:arriba-izquierda (and (> x 0) (> y 0) (not-remember-state-inline? x- y-)
            (let* (
              (arriba-door     (get-bit-list (get-cell-walls x- y)))
              (izquierda-door  (get-bit-list (get-cell-walls x y-)))
              (p3-arriba      (nth 3 arriba-door))
              (p0-izquierda   (nth 0 izquierda-door))
            )
            (or (and (eql 0 p3) (eql 0 p0-izquierda)) (and (eql 0 p0) (eql 0 p3-arriba))))))
        )
      ) )



;;;=======================================================================================
;;  VALID-POSITION (estado)
;;        Predicado.  Indica si una posicion  es valido segun las restricciones del problema
;;;=======================================================================================
(defun  valid-position? (x y)
"Predicado. Valida  un estado según las restricciones generales del problema..."
  (let*  
      (
        (rows (get-maze-rows))        
        (cols (get-maze-cols))   )

        (and (>= x 0) (>= y 0) (< x rows) (< y cols))   ) )


;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================
(defun  apply-operator (op  state) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  
      (
        (coordinates (second state))
        (x (first  coordinates))
        (y (second coordinates))
        (x+ (+ x 1) )
        (x- (- x 1) )
        (y+ (+ y 1) )
        (y- (- y 1) )

        (name (first op))

        (new-coordinates
          (case name
            (:arriba           (list x- y ))
            (:arriba-derecha   (list x- y+))
            (:derecha          (list x  y+))
            (:abajo-derecha    (list x+ y+))
            (:abajo            (list x+ y ))
            (:abajo-izquierda  (list x+ y-))
            (:izquierda        (list x  y-))
            (:arriba-izquierda (list x- y-))
          )
        )
      )

      (list (aptitude new-coordinates) new-coordinates)   ) )


;;;=======================================================================================
;;  EXPAND (state)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (state)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops*"
  (setq  *current-ancestor* state)
  (let 
    ((new-state nil))
    (incf  *expanded*)

    (dolist  (op  *Ops*)
      (cond 
        ((valid-operator? op state)

          (setq  new-state  (apply-operator  op state))
          (incf  *id*)

          (if 
            (< (first new-state) (first *closest*))
            (setq *closest* new-state)
          )
          
          (add-to-memory new-state (second op))
          (insert-to-open new-state)                       )))))



(defun extract-solution (state)
"Rastrea en *memory* todos los descendientes de [state] hasta llegar al estado inicial"
  (let 
    (
      (current state)
      (op nil)
      (ans nil)
      (value nil)
    )

      (loop  while  (not (null current)) do 
        (setq value (get-hash-inline (first (second current)) (second (second current)) ))

        (setq op (gethash value *memory-op*))
        (setq ans (gethash value *memory-an*))

        (setq current ans)
        (push op *solution*)  )

      (setq *solution* (rest *solution*)) ))

(defun  display-solution ()
"Despliega la solución en forma conveniente y numerando los pasos"

    (format  t  "Solución con ~A pasos (Longitud de la solución)~%" (length *solution*))
    (format  t  "~A nodos creados ~%" *id*)
    (format  t  "~A nodos expandidos ~%" *expanded*)
    (format  t  "Longitud máxima de la Frontera de búsqueda: ~A~%~%" *max-frontier*)

    (format  t  "~%La solucion es: ~A ~%" *solution*)

    (format  t  "~%5) Tiempo: ~%")
)



(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
  (setq  *id* -1)                         
  (setq  *open* ())                       
  (setq  *memory-op* (make-hash-table))   
  (setq  *memory-an* (make-hash-table))   
  (setq  *expanded*         0)            
  (setq  *max-frontier*     0)
  (setq  *closest*     '(9999999999 nil))
  (setq  *current-ancestor* nil)  )

     
(defun get-start ()
  "Te regresa el estado inicial"
    (let*
      ( 
        (x (aref  *start* 0))
        (y (aref  *start* 1))
        (coordinate (list x y)  )  )
      (list (aptitude coordinate) coordinate)   ) )

(defun get-goal ()
  "Te regresa el estado inicial"
    (let*
      ( 
        (x (aref  *goal* 0))
        (y (aref  *goal* 1))
        (coordinate (list x y)  )  )
      (list (aptitude coordinate) coordinate)   ) )


(defun  bestf-search ()
"Realiza una búsqueda best First, desde un estado inicial hasta un estado meta"
  (reset-all)

  (print (get-maze-data))
  (print *start*)
  (print *goal*)

  (let 
    (
      (edo-inicial  (get-start))
      (edo-meta     (get-goal))

      (current          nil)
      (sucesores        nil)
      (meta-encontrada  nil)  )

      (insert-to-open edo-inicial)
      (add-to-memory edo-inicial -1)

      (time 
      (loop until (or meta-encontrada (null *open*) ) do
        (setq current (get-from-open))
        (cond    
          ((equal edo-meta current) 
            
            (setq  meta-encontrada  T)
            (format  t  "Éxito. Meta encontrada ~%~%")
            (extract-solution current)
            (display-solution)
          )

          (t
             
            (expand current)
            (if (null *open*) (progn (extract-solution *closest*) (display-solution)) )
          ) ))
          ))  )
			     

(add-algorithm 'bestf-search)

(start-maze)