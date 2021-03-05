#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(print-only-errors true)

(define (prueba e)
  (interp (parse e)))

(test (parse 'foo)              (id 'foo))
(test (parse 1)                 (num 1))
(test (parse '{sub1 1})         (op sub1 (list (num 1))))
(test (parse '{+ 1 1})          (op + (list (num 1) (num 1))))
(test (parse '{with {[a 1]} a}) (with (list (binding 'a (num 1))) (id 'a)))
;; (test (parse '{with* {[a 2] [b {+ a a}]} b}) (with* (list (binding 'a (num 2)) (binding 'b (op + (list (id 'a) (id 'a))))) (id 'b)))

(test (subst (id 'foo) 'foo (num 2)) (num 2))
(test (subst (id 'bar) 'foo (num 2)) (id 'bar))
(test (subst (with (list (binding 'bar (id 'foo)) (binding 'foo (num 3))) (id 'foo))
             'foo
             (num 1))
             (with (list (binding 'bar (num 1)) (binding 'foo (num 3))) (id 'foo)))

(test (subst (with (list (binding 'bar (id 'foo)) (binding 'x (num 3))) (id 'foo))
             'foo
             (num 1))
             (with (list (binding 'bar (num 1)) (binding 'x (num 3))) (num 1)))


(test/exn (prueba 'foo) "interp: Variable libre: 'foo")
(test (prueba '1234) 1234)
(test (prueba '{+ 1 2 3}) 6)
(test (prueba '{- 1 2 3}) -4)
(test (prueba '{* 1 2 -3}) -6)
(test (prueba '{* 1 2 -3 0}) 0)
(test (prueba '{/ 1 2}) (/ 1 2))
(test (prueba '{modulo 3 2}) 1)
(test (prueba '{sub1 3}) 2)
(test (prueba '{sub1 (add1 (expt 3 3))}) 27)
(test/exn (prueba 'a) "interp: Variable libre: 'a")
(test (prueba '{with {[x 2] [y 3]} {+ x 3 y}}) 8)
(test (prueba'{with* {{a 2} {b {+ a a}}} b}) 4)
(test/exn (prueba'{with {{a 2} {b {+ a a}}} b}) "interp: Variable libre: 'a")

;;; Pruebas nuevas
(test (parse 'x) (id 'x))
(test (parse '2) (num 2))
(test (parse '{with ([x 2] [y 3]) ( + x y)}) (with (list (binding 'x (num 2)) (binding 'y (num 3))) (op + (list (id 'x) (id 'y)))))
(test (parse '{sub1 2}) (op sub1 (list (num 2))))
;Esta es deseable, si no la pasa no baja puntos.
(test/exn (parse '{sub1 2 3}) "parser: Aridad incorrecta en la función 'sub1")

(test/exn (prueba 'foo) "interp: Variable libre: 'foo")
(test (prueba '1234) 1234)
(test (prueba '{+ 1 2 3}) 6)
(test (prueba '{- 1 2 3}) -4)
(test (prueba '{* 1 2 -3}) -6)
(test (prueba '{* 1 2 -3 0}) 0)
(test (prueba '{/ 1 2}) (/ 1 2))
(test (prueba '{modulo 3 2}) 1)
(test (prueba '{sub1 3}) 2)
(test (prueba '{sub1 (add1 (expt 3 3))}) 27)
(test/exn (prueba 'a) "interp: Variable libre: 'a")
(test (prueba '{with {[x 2] [y 3]} {+ x 3 y}}) 8)
(test (prueba '{with {{x 5} {y 1}} {+ x y}}) 6)
(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) 6)
(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) 11)
(test/exn (prueba'{with {{a 2} {b {+ a a}}} b}) "interp: Variable libre: 'a")
(test (prueba'{with* {{a 2} {b {+ a a}}} b}) 4)
(test/exn (prueba'{with {{y 1} {x y}} x}) "interp: Variable libre: 'y")
;; Esta prueba tiene un error
;; (test (prueba'{with* {{x y} {y 1}} x}) 1)
(test/exn (prueba'{with* {{x y} {y 1}} x}) "interp: Variable libre: 'y")
(test (prueba '{with {{x 5}}
                     {with {{x 2}}
                           {+ x x}}}) 4)
(test (prueba '{with ([x 2] [y 1] [z 3]) (+ x y z)}) 6)
(test (prueba'{with ([x 2] [y 1] [z 3]) (/ x y z)}) 2/3)

(test (prueba '{with ([x 1]
                      [y 1])
                     {with ([x 5]
                            [y x])
                           (+ x y)}}) 6)
(test (prueba '{with ([x 1]
                      [y 1])
                     {with* ([x 5]
                            [y x])
                           (+ x y)}}) 10)
;Esta es deseable, si no la pasa no baja puntos.
(test/exn (prueba '{with ([x 1]
                      [x 2])
                     {+ x x}})
                            "parse: El símbolo 'x está declarado más de una vez")

(test/exn (prueba'{with {{a 2} {b {+ a a}}} b}) "interp: Variable libre: 'a")
