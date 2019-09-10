;;;; Paquete #1 Oscar Andres Rosas Hernandez

;;;; ====  Seccion 1 =======

;;; 1a: El quinto elemento de la lista (((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))), 
;;;     sin usar la función FIFTH.
(print "1a")
(print 
    (first (rest (rest (rest (rest '( ((1 2) 3) 4 (5 (6)) A (B C) D (E (F G)))))))))

;;; Comprobacion 
(print 
    (fifth '( ((1 2) 3) 4 (5 (6)) A (B C) D (E (F G))))
)
(terpri)


;;; 1b: El número de segundos que tiene el año bisiesto 2004.
(print "1b")
(print 
    ;; days hour  minute  second
    (* 366  24    60      60))
(terpri)

;;; 1c: Si el valor numérico asociado a la variable x es diferente de cero
;;;     y además menor o igual que el valor asociado a la variable y.
(print "1c")
(set 'x 1)
(set 'y 9)

(print 
    (and (not (eql 0 x)) (<= x y) )
)
(terpri)


;2x^2 + 7x + 5 = 0
(print "1d")

(set 'a 2)
(set 'b 7)
(set 'c 5)

(print 
    (list 
        (/ 
            (+ (- b) (sqrt (- (* b b) (* 4 a c) ) )  )
            (* 2 a))

        (/ 
            (- 
                (- b) 
                (sqrt (- (* b b) (* 4 a c) ) )  )
            (* 2 a))))
(terpri)



;;;; ====  Seccion 2 =======

;;; Escriba, en notación prefija y evalúe las siguientes expresiones aritméticas:

(print "2a")
(print (+ (* 2 4) (- 6 8) ) )
(terpri)

(print "2b")
(print (/ (+ 5 -3 4) (+ 6 (/ 2 5) ) ) )
(terpri)

(print "2c")
(print
    (sqrt
        (/
            (+ (- (- 4 (/ 3 8) ) ) 1.4502)
            (expt -1 (expt (- 3 5) (/ 1 3) ) )
        )
    )
)
(terpri)

(print "2d")
(print 
  (expt  
    (/
      (expt  
        (/ 65.402 (sqrt -1) )
        (/ 1 5)
      )
      0.17
    )
    (/ 1 7)
  )
)
(terpri)


;;;; ====  Seccion 3 =======

;;; Indique el resultado de evaluar cada una de las siguientes expresiones:
(print "3a")
(print (cdar '((one two) three four)))
(print '(TWO))
(terpri)

(print "3b")
(print (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven)))
(print '((EVA LISA) KARL SVEN EVA LISA KARL SVEN))
(terpri)

(print "3c")
(print (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin)))
(print '(EVA GITAN LISA GITAN KARIN))
(terpri)

(print "3d")
(print '(remove 'sven '(eva sven lisa sven anna)))
(print '(EVA LISA ANNA))
(terpri)

(print "3e")
(print (butlast '(karl adam nilsson gregg alisson vilma) 3))
(print '(KARL ADAM NILSSON))
(terpri)

(print "3f")
(print (nth 2 '(a b c d e)))
(print 'C)
(terpri)

(print "3g")
(print (nthcdr 2 '(a b c d e)))
(print '(C D E))
(terpri)

(print "3h")
(print (intersection '(a b c) '(x b z c)))
(print '(C B))
(terpri)

(print "3i")
(print (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))))
(print '(4))
(terpri)

;;;; ====  Seccion 4 =======

;;; Defina una función Recombina que reciba como argumento una lista de la forma ((A . x)
;;; (B . y) (C . z)), donde A, B y C son átomos simbólicos, mientras que x, y y z son
;;; números. Como respuesta, la función debe entregar otra lista con la siguiente estructura:
;;; ( ((x y) . A) ((y z) . C) ((z y x) . B) )

(defun recombina 
  (data)

  (let 
    ( 
      (arg1 (first data))
      (arg2 (second data))
      (arg3 (third data))
    )
    (let 
      (
        (A (first arg1))
        (B (first arg2))
        (C (first arg3))

        (x (rest arg1))
        (y (rest arg2))
        (z (rest arg3))
      )

      (let 
        (
          (res1 
            (cons (list x y) A))
          (res2 
            (cons (list y z) C))
          (res3 
            (cons (list z y x) B))
        )
        (list res1 res2 res3)
      )
    )
  )
)

(print "4")
(print (recombina '((A . x) (B . y) (C . z)) ))
(terpri)


;;;; ====  Seccion 5 =======

;;; Defina un predicado RealNoCero? que reciba un argumento N y responda si su
;;; argumento es o no un número real diferente de cero.

(defun realNoCero(N)
  (and 
    (not (eql N 0)) 
    (realp N)
  )
)

(print "5")
(print (realNoCero 'a))
(terpri)


;;;; ====  Seccion 6 =======

;; Construya una función Analiza, con argumento X, que responda una lista con los valores
;; de verdad correspondientes a las respuestas a las siguientes preguntas: ¿es X un átomo?,
;; ¿es X un número?, ¿es X una lista? , ¿es X una celda de construcci ón? y ¿es X una
;; lista vacía?

(defun analiza(X)
  (list 
    (atom X)
    (numberp X)
    (listp X)
    (consp X)
    (null X)
  )
)

(print "6")
(print (analiza 6))
(terpri)

;;;; ====  Seccion 7 =======

;;; Defina una función Intercala que reciba como argumentos dos listas cualesquiera y,
;;; como resultado entregue otra lista en la que se encuentran intercalados los elementos de
;;; las listas originales; siempre en el mismo orden: un elemento de la primera lista y otro de
;;; la segunda lista. Si las listas no tienen la misma longitud, todos los elementos restantes
;;; de la lista más grande se colocan seguidos en la respuesta.

(defun intercala
  (x y)
  (reverse
    (let 
      (
        (result ()) 
        (total (+ (length x) (length y) ) )
      ) 
      (do 
        ( 
          (i 0 (+ i 1)) 
          (j 0 (+ j 1))
        )
        ( (<= total (length result)) result )
        (if (eql (nth i x) nil) nil (push (nth i x) result) )
        (if (eql (nth j y) nil) nil (push (nth j y) result) )
      )
    )
  )
)


(print "7")
(print (intercala  '(X Y) '(A B C D)  ))
(terpri)


;;;; ====  Seccion 8 =======

;;; Programe un predicado MismoTipo que reciba como argumento dos listas de la misma
;;; longitud y como respuesta, devuelva T si ambas listas tienen elementos del mismo
;;; tipo y en las mismas posiciones, NIL en caso contrario. Observe que los elementos no
;;; requieren ser iguales, sólo del mismo tipo de datos.

(defun MiTipo
  (x y)
  (reverse
    (let 
      (
        (result ()) 
        (total (length x))
      ) 
      (do 
        ( (i 0 (+ i 1)) )
        ( (<= total (length result)) result )
        (push 
          (eql (type-of (nth i x)) (type-of (nth i y)) ) 
          result)
      )
    )
  )
)

(print "8")
(print (MiTipo (list 'b 5 ()) (list 'A 3 T) ))
(terpri)

;;;; ====  Seccion 9 =======

;;; Defina una función APalíndromo, sensible a mayúsculas y minúsculas, que reciba como
;;; argumento una cadena y, como respuesta entrega otra cadena que es el palíndromo de la
;;; original. Ejemplo: APalíndromo("Hola") = "HolaaloH".

(defun APalíndromo (str)
  (concatenate 'string str (reverse str))
)

(print "9")
(print (APalíndromo "Hola"))
(terpri)

;;;; ====  Seccion 10 =======

;;; Defina un predicado Bisiesto que reciba como entrada un número entero representando
;;; un año y, como respuesta, indique si se trata de un año bisiesto o no.

(defun Bisiesto (year)
  (or
   (and 
      (zerop (mod year 4))
      (not (zerop (mod year 100)))
    )
    (zerop (mod year 400))
  )
)

(print "10")
(print (Bisiesto 2020))
(terpri)
