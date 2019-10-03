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
(defparameter  *memory-operations* (make-hash-table))    ;; Memoria de operaciones
(defparameter  *memory-ancestor*   (make-hash-table))    ;; Memoria de ancestros
(defparameter  *memory-distance*   (make-hash-table))    ;; Memoria de ancestros
(defparameter  *expanded*          0)                    ;; Cuantos estados han sido expandidos
(defparameter  *max-frontier*      0)                    ;; El tamano de la maximo de la frontera 
(defparameter  *closest*           '(9999999999 nil))    ;; Almacena el estado con la mejor solucion 
(defparameter  *current-ancestor*  nil)                  ;; Almacena al ancestro actual (estado)
(defparameter  *aptitude-id*       nil)                  ;; Almacena el nombre de la funcion


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


(defun delete-from-ordered-list (coordinates states)
  "Elimina un elemento en una lista ordenada en O(n)"
  (if (null states) (return-from delete-from-ordered-list nil))
  (let
    ( 
      (front  (first states))
      (end    (rest states)))
    
      (if (equal coordinates (second front))
        end
        (cons front (delete-from-ordered-list coordinates end)))))


(defun delete-from-open (coordinates)
  "Recibe un estado y lo elimina"
  (setq *open*          (delete-from-ordered-list coordinates *open*)))


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


(defun cost-and-aptitude (coordinates)
  "Te regresa el costo de de aptitud de un nodo, mientras mas pequeño mejor"
    (let
      ( 
        (aptitude-value (aptitude coordinates))
        (distance-value (get-distance (list 0 coordinates))))

        (+ aptitude-value distance-value)
    ) )


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  get-hash-inline  (x y)
  "Te da un ID unico para usarlo como llave en la memoria"
  (+ x (* y (+ 1 (get-maze-rows)))))

(defun  not-remember-state-inline?  (x y)
  "Ya he visto esto antes en memory pero no esta en open"
  (or
    (null (gethash (get-hash-inline x y) *memory-operations*))  
    (not (null (gethash (get-hash-inline x y) *memory-operations*)))
  )
)

(defun  set-distance  (state dis)
  "Anadelo a memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (val         (get-hash-inline x y)) )

    (setf (gethash val *memory-distance*) dis)
  ) )


(defun  get-distance  (state)
  "Anadelo a memoria"

  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (val         (get-hash-inline x y)) 
      (val2        (gethash val *memory-distance*) )
    )

    (if (null val2) 0 val2)

  ) )


(defun  add-to-memory  (state op)
  "Anadelo a memoria"
  (let*  
    ( 
      (coordinates (second  state)) 
      (x           (first  coordinates)) 
      (y           (second coordinates))
      (val         (get-hash-inline x y)) )

    (setf (gethash val *memory-operations*) op)
    (setf (gethash val *memory-ancestor*) *current-ancestor*)

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

      (list (cost-and-aptitude new-coordinates) new-coordinates)   ) )


;;;=======================================================================================
;;  EXPAND (state)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (state)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *operations*"
  (setq  *current-ancestor* state)
  (let*
    (
      (current-coordinates (second state))
      (x (first  current-coordinates))
      (y (second current-coordinates))
      (val       (get-hash-inline x y))
      (new-state nil)
      (pre-value nil)
    )

    (incf  *expanded*)
    (setf (gethash val *memory-operations*) Nil)  ; Ya no estoy en open
    (setf (gethash val *memory-operations*) Nil)  ; Ya no estoy en open

    (dolist  (op  *operations*)
      (cond 
        ((valid-operator? op state)

          (setq  new-state  (apply-operator  op state))
          (incf  *id*)

          (if 
            (< (first new-state) (first *closest*))
            (setq *closest* new-state)
          )

          (setq current-coordinates (second new-state))

          (setq x (first  current-coordinates))
          (setq y (second current-coordinates))
          (setq val       (get-hash-inline x y))
          (setq pre-value (gethash val *memory-operations*))


          (set-distance 
            new-state
            (+ 1 (get-distance *current-ancestor*))
          )

          (setq  new-state  (apply-operator  op state))

          (cond
            ((null pre-value)
              (add-to-memory new-state (second op))
              (insert-to-open new-state)
            )
            ((and
              (not (null pre-value))
              (< pre-value (first new-state))
            )
            (progn  ; Vamos a cambiarlo
              (add-to-memory new-state (second op))

              (delete-from-open (second new-state))
              (insert-to-open new-state)
            ))
          )

          


                                 )))

                                 
                                 
                                 ))



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

        (setq op (gethash value *memory-operations*))
        (setq ans (gethash value *memory-ancestor*))

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
  (setq  *id*                 -1)                         
  (setq  *open*               ())                       
  (setq  *memory-operations*  (make-hash-table))   
  (setq  *memory-ancestor*    (make-hash-table))   
  (setq  *memory-distance*    (make-hash-table))   
  (setq  *expanded*            0)            
  (setq  *max-frontier*        0)
  (setq  *closest*            '(9999999999 nil))
  (setq  *current-ancestor*   nil))
     
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


(defun  a*-manhattan-search ()
"Realiza una búsqueda A*, desde un estado inicial hasta un estado meta"
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
      (i 0)
      (meta-encontrada  nil)  )

      (insert-to-open edo-inicial)
      (add-to-memory edo-inicial -1)
      (set-distance edo-inicial 0)

      (time 
      (loop until (or meta-encontrada (null *open*)) do
        (setq current (pop  *open*))

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
			     

(add-algorithm 'a*-manhattan-search)
(a*-manhattan-search)
(start-maze)