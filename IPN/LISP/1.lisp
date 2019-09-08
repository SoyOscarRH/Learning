;;;; Paquete #1

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

(print (recombina '((A . x) (B . y) (C . z)) ))