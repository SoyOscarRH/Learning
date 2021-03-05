#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(print-only-errors true)

(define (prueba e)
  (interp (parse e) (mtSub)))

;; Pruebas propias

;; Parse
(test (parse '{{fun {x} {+ x 4}} {5}})  (app (fun '(x) (op + (list (id 'x) (num 4)))) (list (num 5))))
(test (parse '{{fun {x y} {+ x 4 y}} {5 5}})  (app (fun '(x y) (op + (list (id 'x) (num 4) (id 'y)))) (list (num 5) (num 5))))
(test (desugar-with (with (list (binding 'a (num 1))) (id 'a)))
      (app (fun '(a) (id 'a)) (list (num 1))))

(test (parse '{with {[a 1] [b {with {[a 2]} a}]} a})
      (app (fun '(a b) (id 'a)) (list (num 1) (app (fun '(a) (id 'a)) (list (num 2))))))

;; Pruebas del laboratorio
;; Parse
(test/exn (parse '{fun {x y x} {+ x {+ y z}}})
      "parser: parámetro definido dos veces: x")

(test (parse '{{fun {x y} {+ x y}} {10 8}})
      (app (fun '(x y) (op + (list (id 'x) (id 'y)))) (list (num 10) (num 8))))

(test (parse '{with {{x 1} {y x} {z 3}} x})
      (app (fun '(x y z) (id 'x)) (list (num 1) (id 'x) (num 3))))

(test (parse '{with {{x 1}
                     {y 2}}
                    {with {{z 4}
                           {w 5}}
                          {with {{f {fun {x} x}}}
                                {f {3}}}}})
      (app (fun '(x y)
                (app (fun '(z w)
                          (app (fun '(f)
                                    (app (id 'f)
                                         (list (num 3))))
                               (list (fun '(x) (id 'x)))))
                     (list (num 4) (num 5))))
           (list (num 1) (num 2))))

(test (parse '{{fun {x y} {+ {* x y} x}} {3 4}})
      (app (fun '(x y) (op + (list (op * (list (id 'x) (id 'y))) (id 'x)))) (list (num 3) (num 4))))

(test/exn (parse '{{fun {x y} {+ x y}} {10}})
      "parser: La cardinalidad de los argumentos difiere de la aridad de la función")


;; Interp
(test (prueba '{if0 {- 1 1} 5 6}) (numV 5))

(test (prueba '{if0 {+ 1 2} 5 6}) (numV 6))
(test (prueba '{if0 {+ 1 2} 5 6}) (numV 6))

;Pruebas de función
(test/exn (prueba '{fun {x y x} {+ x {+ y z}}})
      "parser: parámetro definido dos veces: x")

(test (prueba '{fun {x y z} {+ x {+ y z}}})
      (closure '(x y z) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (mtSub)))

;Pruebas de aplicación de función
(test (prueba '{{fun {x y} {+ x y}} {10 3}}) (numV 13))

(test (prueba '{{fun {x y} {+ {* x y} x}} {3 4}}) (numV 15))

;Pruebas de with
(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))
(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x")
(test (prueba '{with {{f {fun {x} {+ x x}}}} {f {3}}}) (numV 6))
(test/exn (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")


(define myparse (parse '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}))
(define myfun (app-fun myparse))
(define myfunval (interp myfun (mtSub)))
(define myargs (app-args myparse))

(define myparse2 (parse '{with {{x 3} {y x}} y}))
(define myfun2 (app-fun myparse2))
(define myfunval2 (interp myfun2 (mtSub)))
(define myargs2 (app-args myparse2))

;Pruebas de with*
(test (prueba '{with* {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f {10}}}}}
                      {+ x z}}) (numV 20))

(test (prueba '{with* {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f {1}}}}}) (numV 4))

(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6))

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))

(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y z} {+ x {+ y z}}}})
      (closure '(x y z) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (numV 3) (aSub 'y (numV 2) (aSub 'x (numV 1) (mtSub))))))

(test/exn (prueba '(with* ([y {+ x x}] [x 1]) y)) "lookup: Hay un identificador libre: x")

(test (prueba '(with* ([x 2] [y {+ x x}] [x 1]) (+ 0 y))) (numV 4))

(test (prueba '(with* ([x 2] [y {+ x x}] [x 1]) (+ x y))) (numV 5))

