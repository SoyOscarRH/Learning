;;;======================================================================================
;;;  GLOL.lisp
;;;      Resuelve el problema de Granjero, lobo, oveja y legumbres con 
;;;      búsqueda ciega: a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados:
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla, número de lobo(B), oveja(O), legumbres(L), la barca esta en esta orilla(A).
;;;                 Estado inicial:               Estado meta:
;;;                 B O L A   B   O   L   A           B   O   L   A     B O L A 
;;;               ((T T T T) (nil nil nil nil))     ((nil nil nil nil) (T T T T))
;;;
;;;      Oscar Andres Rosas Hernandez
;;;======================================================================================
(defparameter  *open* ())    ;; Frontera de busqueda...                                              
(defparameter  *memory* ())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:regresar-bote-solo   -1 )
                         (:mover-lobo            0 )
                         (:mover-oveja           1 )
                         (:mover-legumbre        2 ) ) )

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *expanded*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *maxima-frontera*  0)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)  -> (id estado ancestro operation-name)
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (setq *maxima-frontera* (max (+ 1 (length *open*)) *maxima-frontera*))
         (cond 
            ((eql  metodo  :depth-first)
	            (push  nodo  *open*))
	          ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
     (if  (fourth (first  estado))  0  1))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado..."  
  (let*  
      (
        (orilla   (barge-shore  estado))                     
        (a-mover  (second op))  )

        (or  
          (eql a-mover -1)                    ;; Si es que solo mueves la barca, siempre es valido 
          (nth a-mover (nth orilla estado)) ) ;; Si es que hay algo ahi, va, es un operador valido
        ) ) 

;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en la orilla en la que estan solas las cosas no este el lobo y oveja
;;                          o oveja y legumbre
;;;=======================================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema..."
  (let* 
    (
      (uno     (first   estado))   
      (dos     (second  estado)) 

      (lobo-uno     (first  uno))   
      (oveja-uno    (second uno))
      (legumbre-uno (third  uno))
      (lobo-dos     (first  dos))
      (oveja-dos    (second dos))
      (legumbre-dos (third  dos))

      (bote-uno (fourth  uno))
      (bote-dos (fourth  dos))

    )
    
    (not 
      (or  
      (and lobo-uno oveja-uno (not bote-uno)) 
      (and lobo-dos oveja-dos (not bote-dos))
      (and oveja-uno legumbre-uno (not bote-uno))
      (and oveja-dos legumbre-dos (not bote-dos)) ) ) ) ) 

;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================
(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  
      (
        (orilla-barca (barge-shore estado))
        (orilla-no-barca (flip (barge-shore estado)))
        (origen   (copy-list (nth orilla-barca estado)))
        (destino  (copy-list (nth orilla-no-barca estado)))

        (place    (second op))  ;; este operador es el indice en el estado
      )

      (if 
        (not (eql place -1))
        (setf (nth place origen) nil) );; lo mueve de aqui

      (if 
        (not (eql place -1))
        (setf (nth place destino) T) )  ;; para ponerlo aqui


      (setf (nth 3 origen) nil)     ;; la barca no esta aqui
      (setf (nth 3 destino) T)      ;; ahora esta qui

      (if (eql orilla-barca 0)   
        (list origen destino)
        (list destino origen)
      )         ) )            


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
            (incf  *expanded*)
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado)           ;; se valida el resultado...
			    (valid-state?  nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )


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
    (format  t  "4) Longitud máxima de la Frontera de búsqueda: ~A~%~%" *maxima-frontera*)
    (let  ((nodo  nil))
        (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
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
     (setq  *maxima-frontera*  0) 
     (setq  *solucion*  nil))


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...



	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada ~%~%")

		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...

			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )
			     
     
;;;=======================================================================================
;;;=======================================================================================
(format  t  "===========================~%Breath-first: ~%~%")
(time (blind-search
	'((T T T T) (nil nil nil nil))     
	'((nil nil nil nil) (T T T T))
	:breath-first))


(format  t  "===========================~%Depth-first: ~%~%")
(time (blind-search
  '((T T T T) (nil nil nil nil))     
	'((nil nil nil nil) (T T T T))
	:depth-first))