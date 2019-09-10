;;;; Paquete #2 Oscar Andres Rosas Hernandez

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

;;;; ====  7 =======
;;; Defina una función Aplana que reciba como argumento una lista con elementos anidados a
;;; cualquier nivel de profundidad y, como respuesta, entregue una lista conteniendo los mismos
;;; elementos pero todos ellos al nivel principal de profundidad.

(defun Aplana (lista)
  (cond 
    ((null lista) nil)
    ((atom lista) (list lista))
    (t (loop for elem in lista appending (Aplana elem))))
)

(print "7")
(print (Aplana '(2 C 0 nil 1 (3 2) B)))
(terpri)

;;;; ==== 8 =======
;;; Escriba la función Diagonal que recibe como argumento una lista conteniendo m sub-listas de
;;; m elementos cada una de ellas y que representa una matriz de m x m elementos. Como
;;; respuesta, esta función debe devolver una lista conteniendo los elementos en la diagonal
;;; principal de dicha matriz. Observe que los elementos de la matriz pueden ser de cualquier tipo
;;; de datos, no forzosamente numéricos.

(defun Diagonal (matrix)
  (cond 
    ((null matrix) nil)
    (t 
      (append 
        (list (first (first matrix)))
        (Diagonal 
          (loop for elem in (rest matrix) collect (rest elem))
        )
      )
    )
  )
  
)

(print "8")
(print (Diagonal 
  '(
    (1 A 3)
    (4 nil 6)
    (C 8 D)
  )
))
(terpri)

;;; ==== 9 =======
;;; Construya una función que reciba como argumento una lista cualquiera y, como respuesta,
;;; entregue una lista, con el mismo número de elementos de primer nivel, pero que contiene un
;;; símbolo A si el elemento en la posición correspondiente es un átomo, un símbolo L si el
;;; elemento correspondiente es una lista y un símbolo N si el elemento en la posición
;;; correspondiente es una lista vacía.

(defun Types (lista)
  (cond 
    ((null lista) nil)
    ((null (first lista)) (append  (list 'N)  (Types (rest lista))))
    ((atom (first lista)) (append  (list 'A)  (Types (rest lista))))
    ((listp (first lista)) (append (list 'L)  (Types (rest lista))))
  )
  
)

(print "9")
(print (Types '(4 nil (1 2) 3)))
(terpri)

;;; ==== 10 =======
;;; Defina la función Suma-numérica que recibe como argumento una lista cualquiera (no
;;; anidada), y como respuesta entrega la suma de exclusivamente aquellos elementos de la lista que
;;; son numéricos.

(defun Suma-numérica (nums)
  (cond 
    ((null nums) 0)
    ((numberp (first nums)) 
      (+ (first nums) (Suma-numérica (rest nums)))
    )
    (t (Suma-numérica (rest nums)))
  )
  
)

(print "10")
(print (Suma-numérica '(4 nil (1 2) 3)))
(terpri)

;;; ==== 11 =======
;;; Escriba una función Filtra-vocales que reciba como argumento una lista (con elementos de
;;; cualquier tipo y anidada a cualquier nivel de profundidad) y, como respuesta entregue una copia
;;; de la lista argumento en la cual se han removido las letras vocales (tanto mayúsculas como
;;; minúsculas).

(defun vocal-p (char) (find char "aeiou" :test #'char-equal))

(defun Filtra-vocales (lista)
  (cond 
    ((null lista) nil)
    ((stringp (first lista)) 
      (cons 
        (remove-if #'vocal-p (first lista))
        (Filtra-vocales (rest lista))
      )
    )
    ((and (characterp (first lista)) (vocal-p (first lista)))   
      (Filtra-vocales (rest lista))
    )
    ((atom (first lista))
      (cons (first lista) (Filtra-vocales (rest lista)))
    )
    ((listp (first lista))
      (list (Filtra-vocales (first lista)) (Filtra-vocales (rest lista)))
    )
  )
  
)

(print "11")
(print (Filtra-vocales '(4 "hello" nil (#\A 2) #\C 3 #\d #\E)))
(terpri)

;;; ==== 12 =======
;;; Construya una función Filtra-múltiplos que reciba como argumentos una lista y un número
;;; entero. Como respuesta debe entregar una copia de la lista argumento en la cual se han
;;; removido todos los múltiplos del entero recibido.

(defun Filtra-múltiplos (lista num)
  (cond 
    ((null lista) nil)
    ((and (numberp (first lista)) (eq 0 (mod (first lista) num)))
      (Filtra-múltiplos (rest lista) num)
    )
    (t
      (cons (first lista) (Filtra-múltiplos (rest lista) num))
    )
  )
)

(print "12")
(print (Filtra-múltiplos '(4 "hello" C 3 d 9 11) 3))
(terpri)

;;; ==== 13 =======
;;; Defina la función Celdas que recibe como argumento una lista (con elementos de cualquier tipo
;;; y anidada a cualquier nivel de profundidad) y, como respuesta entrega el número de celdas de
;;; construcción que contiene la representación interna de la lista argumento.

(defun Celdas (lista) 
  (cond 
    ((null lista) 0)
    ((atom (first lista))
      (+ 1 (Celdas (rest lista)))
    )
    (t
      (+ (Celdas (first lista)) (Celdas (rest lista)))
    )
  )
)

(print "13")
(print (Celdas '(4 "hello" C (3 3) d 9 11)))
(terpri)

;;; ==== 14 =======
;;; Construya una función Implica con aridad indeterminada, que implemente el operador lógico de
;;; la implicación.
;;; p -> q -> r

(defun sin-ultimo(lista)
  (reverse (rest (reverse list)))
)

(defun Implica (p q)
  (if (and p (not q)) 
    nil
    T
  )
)

(print "14")
(print (Implica T nil))
(terpri)


;;; ==== 15 =======
;;; Construya una función Implica con aridad indeterminada, que implemente el operador lógico de
;;; la implicación.
;;; p -> q -> r

(defun mmat (mat1 mat2)
           "multiply two matrices as lists of lists"
           (loop with num-rows1 = (length mat1)
              for row1 in mat1
              collect (loop for c from 0 below num-rows1
                         collect (loop for e in row1
                                    for r from 0
                                    sum (* e (nth c (nth r mat2)))))))


(print "15")
(print (mmat '((1 21 3)(41 9 6)) '((7 8)(91 10)(11 12))))
(terpri)



                                    