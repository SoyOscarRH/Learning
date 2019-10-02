(load "maze_lib.lisp")

;;; BestF.lisp
;;;   Resuelve el problema de los laberintos
;;;
;;;   Representación de los estados:
;;;     Lista con dos elementos: Un valor de aptitud y una lista (x, y) de su posicion
;;;
;;; Oscar Andres Rosas Hernandez

(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos

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

        (name (first op))

        (doors 
          (if (valid-position? x y)
            (get-bit-list (get-cell-walls x y))
            nil )         )
        (p0 (first  doors))
        (p1 (second doors))
        (p2 (third  doors))
        (p3 (fourth doors))
        
        (rows (get-maze-rows))        
        (cols (get-maze-cols))   )

        (case name
          (:arriba           (and (> x 0)               (eql p0 0)) )
          (:arriba-derecha   (and (> x 0) (< y cols)    (or (eql p0 0) (eql p1 0))) )
          (:derecha          (and (< y cols)            (eql p1 0)) )
          (:abajo-derecha    (and (< x rows) (< y cols) (or (eql p1 0) (eql p2 0))) )
          (:abajo            (and (< x rows)            (eql p2 0)) )
          (:abajo-izquierda  (and (< x rows) (> y 0)    (or (eql p2 0) (eql p3 0))) )
          (:izquierda        (and (> y 0)               (eql p3 0)) )
          (:arriba-izquierda (and (> x 0) (> y 0)       (or (eql p3 0) (eql p0 0)) ))
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
     (let 
      ((descendests nil)
	     (new-state   nil))

        (incf  *expanded*)
        (dolist  (op  *Ops*)
          (cond ( (valid-operator? op state)
              (setq  new-state  (apply-operator  op state))
              (setq  descendests  (cons (list new-state op) descendests)
              
            ) )     ) )
            
        descendests ) )









;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

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
    (let  ((nodo  nil))
        (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
        (push (nth 4 nodo) *solution*)
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))
       
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


(defun  blind-search (edo-inicial  edo-meta)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta"
  (reset-all)
  (let 
    (
      (nodo             nil)
      (estado           nil)
      (sucesores        nil)
      (operador         nil)
      (meta-encontrada  nil)  )

      (insert-to-open edo-inicial nil)

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
          ) )))  )
			     
     
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

(blind-search (get-start) (get-goal))
(setq *solution* (reverse *solution*))
(pop *solution*)
(print *solution*)
