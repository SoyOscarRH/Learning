(load "maze_lib.lisp")

;;; BestF.lisp
;;;   Resuelve el problema de los laberintos
;;;
;;;   Representación de los estados:
;;;     Lista con dos elementos: Un valor de aptitud y una lista (x, y) de su posicion
;;;
;;; Oscar Andres Rosas Hernandez

(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* (make-hash-table))  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:arriba           0 )
                         (:arriba-derecha   1 )
                         (:derecha          2 )
                         (:abajo-derecha    3 )
                         (:abajo            4 )
                         (:abajo-izquierda  5 )
                         (:izquierda        6 )
                         (:arriba-izquierda 7 )  ) )


(defparameter  *id*               -1)    ;; Identificador del ultimo nodo creado
(defparameter  *expanded*         0)     ;; Almacena cuantos nodos fueron creados
(defparameter  *max-frontier*     0)     ;; Almacena el tamano de la maximo de la frontera 
(defparameter  *current-ancestor* nil)   ;; Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*         nil)   ;; lista donde se almacenará la solución recuperada de la memoria

;;;=======================================================================================
;;  CREATE-NODE (state  op)  
;;      state - Un estado del problema a resolver (sistema)  -> (id estado ancestro operation-name)
;;         op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (state  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  state  *current-ancestor*  (first op) (second op)  )  )  ;;los nodos generados son descendientes de *current-ancestor*


;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  Recibe un estado, una operacion con el que llegamos ahi y lo inserta
;;                        segun aptitud
;;;=======================================================================================
(defun push-to-ordered-list (value node some_list)
  (let
    ( 
      (front (first some_list))
      (end    (rest some_list)) )

    (if (null some_list)
      (cons node nil)
      (if (<= value (first front))
          (cons node some_list)
          (cons front (push-to-ordered-list value node end) ))) ) )

(defun insert-to-open (estado  op) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let 
        (
          (node               (create-node  estado  op))
          (current-frontier   (+ 1 (length *open*)))  )

            (setq *open*          (push-to-ordered-list (first node) node *open*))
            (setq *max-frontier*  (max current-frontier *max-frontier*))   ) )

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
  (pop  *open*) )


;;;=======================================================================================
;;  APTITUDE 
;;      (x y) - Un estado del problema
;;;=======================================================================================
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

(defun get-bit-list (cell-id)
  "Te una lista para saber si la puerta esta cerrada"
    (loop for i below 4 collect (if (logbitp i cell-id) 1 0)) )


;;;=======================================================================================
;;  VALID-OPERATOR [op, state]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun 
;;                    los recursos en la orilla de la barca
;;;=======================================================================================
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
          (:arriba           (and (> x 0)                 (eql p0 0)) (remember-state? x- y) )
          (:derecha          (and (< y+ cols)             (eql p1 0)) (remember-state? x y+) )
          (:abajo            (and (< x+ rows)             (eql p2 0)) (remember-state? x+ y) )
          (:izquierda        (and (> y 0)                 (eql p3 0)) (remember-state? x y-) )

          (:arriba-derecha   (and (> x 0) (< y+ cols) (remember-state? x- y+)
            (let* (
              (derecha-door (get-bit-list (get-cell-walls x y+)))
              (arriba-door  (get-bit-list (get-cell-walls x- y)))
              (p0-derecha (nth 0 derecha-door))
              (p1-arriba  (nth 1 arriba-door))
            )
            (or (and (eql 0 p0) (eql 0 p1-arriba)) (and (eql 0 p1) (eql 0 p0-derecha))))))

          (:abajo-derecha    (and (< x+ rows) (< y+ cols) (remember-state? x+ y+)
            (let* (
              (derecha-door (get-bit-list (get-cell-walls x y+)))
              (abajo-door   (get-bit-list (get-cell-walls x+ y)))
              (p1-abajo   (nth 1 abajo-door))
              (p2-derecha (nth 2 derecha-door))
            )
            (or (and (eql 0 p1) (eql 0 p2-derecha)) (and (eql 0 p2) (eql 0 p1-abajo))))))

          (:abajo-izquierda  (and (< x+ rows) (> y 0) (remember-state? x+ y-)
            (let* (
              (izquierda-door (get-bit-list (get-cell-walls x y-)))
              (abajo-door     (get-bit-list (get-cell-walls x+ y)))
              (p2-izquierda (nth 2 izquierda-door))
              (p3-abajo     (nth 3 abajo-door))
            )
            (or (and (eql 0 p2) (eql 0 p3-abajo)) (and (eql 0 p3) (eql 0 p2-izquierda))))))

          (:arriba-izquierda (and (> x 0) (> y 0) (remember-state? x- y-)
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
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  get-hash?  (x y)
  (+ x (* y (+ 1 (get-maze-cols))))
)

(defun  remember-state?  (x y)
  (gethash (get-hash? x y) *memory*)
)

(defun  add-to-memory  (x y)
  (gethash (get-hash? x y) *memory*)
  (setf (gethash (get-hash? x y) *memory*) T)
)


;;;=======================================================================================
;;  EXPAND (state)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (state)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops*"
     (let 
      ((descendests nil)
	     (new-state   nil))

        (incf  *expanded*)
        (dolist  (op  *Ops*)
          (cond ( (valid-operator? op state)
              (setq  new-state  (apply-operator  op state))
              (print state)
              (add-to-memory (second (first state)) (second (second state)))
              (setq  descendests  (cons (list new-state op) descendests)
              
            ) )     ) )
            
        descendests ) )



;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))

(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "1) Solución con ~A pasos (Longitud de la solución)~%" (1- (length  lista-nodos)))
    (format  t  "2) ~A nodos creados ~%" *id*)
    (format  t  "3) ~A nodos expandidos ~%" *expanded*)
    (format  t  "4) Longitud máxima de la Frontera de búsqueda: ~A~%~%" *max-frontier*)

    (setq *solution* nil)
    (setq *solution* (reverse *solution*))
    (pop *solution*)
    
    (format  t  "~%Solution list: ~A ~%" *solution*)
       
    (format  t  "~%5) Tiempo: ~%")
)  ;; imprimir el número de paso, operador y estado...


;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  ())
     (setq  *memory*  ())
     (setq  *id*  -1)
     (setq  *current-ancestor*  nil)
     (setq  *expanded*  0)  
     (setq  *max-frontier*  0) 
     (setq  *solucion*  nil))


     
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
      (nodo             nil)
      (estado           nil)
      (sucesores        nil)
      (operador         nil)
      (meta-encontrada  nil)  )

      (insert-to-open edo-inicial nil)

      (time 
      (loop until (or meta-encontrada (null *open*)) do
        
        (setq 
            nodo      (get-from-open)       ;;Extraer el siguiente nodo de la frontera de búsqueda
            estado    (second nodo)         ;;Identificar el estado y operador que contiene
            operador  (third  nodo)   )             
        (push nodo *memory*)                ;;Recordarlo antes de que algo pueda pasar...

        (cond    
          ((equal  edo-meta estado)  
            (format  t  "Éxito. Meta encontrada ~%~%")

            (display-solution  (extract-solution  nodo))
            (setq  meta-encontrada  T)    
          )
          (t
            (setq  *current-ancestor* (first  nodo)) 
            (setq  sucesores          (expand estado))
            (setq  sucesores          (filter-memories  sucesores))    ;;Filtrar los estados ya revisados...

            (loop for element in sucesores do
              (insert-to-open (first element) (second element)))
          ) ))))  )
			     

(add-algorithm 'bestf-search)
(bestf-search)

(start-maze)

