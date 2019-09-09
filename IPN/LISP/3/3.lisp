;;;; Paquete #3

;;;; ====  1 =======
;;; [sin usar elt ni position] Defina una función ElemInPos
;;; que reciba tres argumentos: elem, lista y pos. 
;;; La función debe devolver T si elem está en la posición pos de
;;; lista, NIL si no lo está.

(defun ElemInPos (elem lista pos)
  (cond
    ((null lista) 
      nil)
    ((eql pos 0) 
      (eql (first lista) elem))
    (t                  
      (ElemInPos elem (rest lista) (- pos 1) ))))

(print "1")
(print (ElemInPos 3 '(1 2 3) 1))
(terpri)

;;;; ====  2 =======
;;; Escriba la función Inicio-en que recibe como argumentos una lista y un elemento cualquiera. La
;;; función debe entregar como respuesta una copia de la lista original pero comenzando con la
;;; primera ocurrencia del elemento dado en la lista original.

(defun Inicio-en (lista elem)
  (cond
    ((null lista) nil)
    ((eql (first lista) elem) lista)
    (t                  
      (Inicio-en (rest lista) elem ))))

(print "2")
(print (Inicio-en '(1 3 A B) 3))
(terpri)

;;;; ====  3 =======
;;; Modifique la función del ejercicio anterior para que se llame Termina-en y entregue como
;;; respuesta una copia de la lista original pero que termina en la última ocurrencia del elemento
;;; dado

(defun Termina-en (lista elem)
  (reverse (Inicio-en (reverse lista) elem)))

(print "3")
(print (Termina-en '(1 C nil 3 A B) 3))
(terpri)

;;;; ====  4 =======
;;; Construya una función Primer-impar que reciba como argumento una lista y como respuesta
;;; entregue otra lista conteniendo el primer elemento de la lista original que sea un número impar
;;; y la posición (índice) donde se encuentra. Observe que los elementos de la lista pueden ser de
;;; cualquier tipo de datos.

(defun Primer-impar (lista)
  (cond
    ((null lista) nil)
    ((and (numberp (first lista)) (oddp (first lista))) (list (first lista) 0) )
    (t            
      (let 
        ((res (Primer-impar (rest lista))))
        (if (eq nil res)
          nil
          (list (first res) (+ 1 (second res)))
        )
      )
      )))

(print "4")
(print (Primer-impar '(2 C 0 nil 3 A B)))
(terpri)

;;;; ====  6 =======
;;; Escriba la función Conteo que recibe como argumento una lista cualquiera y, como respuesta,
;;; entregue una celda de construcción cuya primera parte contiene el conteo de elementos
;;; numéricos de la lista original y cuya segunda parte contiene el conteo de sublistas contenidas en
;;; la lista original.

(defun Conteo (lista)
  (cond
    ((null lista) (list 0 0) )
    ((numberp (first lista)) 
      (let
        ((res (Conteo (rest lista))))
        (list 
          (+ (first res) 1)  
          (second res)  
        )
      )
    )
    ((listp (first lista)) 
      (let
        ((res (Conteo (rest lista))) (res_int (Conteo (first lista))))
        (list 
          (first res)  
          (+ (first res_int) (second res))  
        )
      )
    )
    (t (Conteo (rest lista)))
  )
)

(print "6")
(print (Conteo '(2 C 0 nil 1 (3 2) B)))
(terpri)