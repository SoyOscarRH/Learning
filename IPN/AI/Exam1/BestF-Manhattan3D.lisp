(load "maze_lib.lisp")

;;; BestF.lisp
;;;   Resuelve el problema de los laberintos usando best first search
;;;
;;;   Representación de los estados:
;;;     Lista con dos elementos: Un valor de aptitud y una lista (x, y, z) de su posicion
;;;     (aptitud (x y z)) 
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
                         (:derecha          2 )
                         (:abajo            4 )
                         (:izquierda        6 ) ) )


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
(defun  get-hash-inline  (x y z)
  "Te da un ID unico para usarlo como llave en la memoria"
  (+(* 2 (+ x (* y (+ 1 (get-maze-rows))))) z))

(defun  not-remember-state-inline?  (x y z)
  "Ya he visto esto antes?"
  (null (gethash (get-hash-inline x y z) *memory-op*))  )

(defun  add-to-memory  (state op)
  "Anadelo a memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (z           (third coordinates))
      (val         (get-hash-inline x y z)) )

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
    (if (or (eql cell-id 17) (eql cell-id 16)) (setq cell-id 0))
    (loop for i below 4 collect (if (logbitp i cell-id) 1 0)) )



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

(defun  valid-operator? (op state)
"Predicado. Valida la aplicación de un operador a un estado, si no es valido regresa nil"  
  (let*  
      (
        (name (first op))
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
        (return-from valid-operator? nil)   )

      (let*
        (
          (door  (get-cell-walls x1 y1))
          (door2 (get-cell-walls x2 y2))

          (door-data (get-bit-list door))
          
          (rows (get-maze-rows))        
          (cols (get-maze-cols))

          (vertical  (eql (rem (second op) 4) 0))
        )
          
          (if (and (eql door 16) (eql z1 1) vertical)
            (return-from valid-operator? nil)
          )

          (if (and (eql door 16) (eql z1 0) (not vertical))
            (return-from valid-operator? nil)
          )

          (if (and (eql door 17) (eql z1 0) vertical)
            (return-from valid-operator? nil)
          )

          (if (and (eql door 17) (eql z1 1) (not vertical))
            (return-from valid-operator? nil)
          )


          (if (> door 15)
            (setq door (if vertical 10 5))
          )


          (print "vertical")
          (print vertical)

          (if 
            (not (case name
              (:arriba     (eql (nth 0 door-data ) 0))
              (:derecha    (eql (nth 1 door-data ) 0))
              (:abajo      (eql (nth 2 door-data ) 0))
              (:izquierda  (eql (nth 3 door-data ) 0))
            ))
            (return-from valid-operator? nil)
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


          (if (not-remember-state-inline? x2 y2 z2)
          (list (aptitude (list x2 y2 z2) ) (list x2 y2 z2)) nil)

      ) ) )







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
      (setq  new-state  (valid-operator? op state))
      (cond
        ((not (null new-state))
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
        (setq value (get-hash-inline (first (second current)) (second (second current))  (third (second current)) ))

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
        (z 0)
        (coordinate (list x y z)  )  )
      (list (aptitude coordinate) coordinate)   ) )

(defun get-goal ()
  "Te regresa el estado inicial"
    (let*
      ( 
        (x (aref  *goal* 0))
        (y (aref  *goal* 1))
        (z 0)
        (coordinate (list x y z)  )  )
      (list (aptitude coordinate) coordinate)   ) )


(defun  bestf-manhattan-search ()
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
            (if (null *open*) 
              (progn 
                (format  t  "Lo intenté.%")
                (extract-solution *closest*) 
                (display-solution)
              ) )
          ) )) ))  )
			     

(add-algorithm 'bestf-manhattan-search)

;(bestf-manhattan-search)
;(print (VALID-OPERATOR? '(:ABAJO 4) '(3.1622777 (4 2 1)) ))


(start-maze)