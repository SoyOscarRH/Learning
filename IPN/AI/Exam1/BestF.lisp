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
      (list  *id*  state  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*


;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun push-to-ordered-list (value node some_list)
  (let
    ( 
      (front (first some_list))
      (end    (rest some_list)) )

    (if (null some_list)
      (cons node nil)
      (if (<= node front)
          (cons node some_list)
          (cons front (push-to-ordered-list node end) )))
  )
)


(print (push-to-ordered-list 0 '((1 0) (2 3) (3 7))))