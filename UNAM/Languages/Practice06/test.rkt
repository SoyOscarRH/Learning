#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))
(require (file "./verificador.rkt"))
(require (file "./interp.rkt"))

(print-only-errors true)

;; Función auxiliar para pruebas, llama a parse y a typeof
;; con el contexto de tipos vacío.
;; test: SCFWBAE -> Type
(define (prueba-tipo exp)
  (typeof (parse exp) (phi)))

(define (prueba-interp exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (begin (prueba-tipo exp)
           (interp (desugar (parse exp)) (mtSub)))))


(display "PRUEBAS TYPEOF\n______________________________________________________________________________________________________________________________\n\n")

(test (prueba-tipo '#t) (booleanT))

(test (prueba-tipo '2) (numberT))

(test (prueba-tipo '{+ 1 2}) (numberT))

(test/exn (prueba-tipo '{+ 1 #f}) "typeof: Error in parameter (boolS #f)\nExpected type: (numberT)\nGiven type: (booleanT)")

(test/exn (prueba-tipo '{and 1 #f}) "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)")
                  
(test (prueba-tipo '{if #t 2 3}) (numberT))

(test/exn (prueba-tipo '{if #t 2 #t}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test/exn (prueba-tipo '{if 3 2 #t}) "typeof: Type error\nConditional's test-expr must be a boolean\nGiven: (numberT)")

(test (prueba-tipo '{with {{x : number 2} {y : boolean #t} {z : number 1}} {if y x z}}) (numberT))

(test/exn (prueba-tipo '{cond {#t 2} {#f 3} {else #t}}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test (prueba-tipo '{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}}) (funT (list (numberT) (booleanT) (numberT))))

(test/exn (prueba-tipo '{fun {{x : number} {y : number}} : (number number -> number) {if y x 0}}) "typeof: Type error\nConditional's test-expr must be a boolean\nGiven: (numberT)")

(test (prueba-tipo '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 #t}}) (numberT))

(test/exn (prueba-tipo '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 3}})
          "typeof: Type error:\nParameter's type doesn't match expected types\nGiven: (numberT)\nExpected: (booleanT)")

(display "PRUEBAS INTERP\n______________________________________________________________________________________________________________________________\n\n")
(test (prueba-interp '#t) (boolV #t))

(test (prueba-interp '2) (numV 2))

(test (prueba-interp '{+ 1 2}) (numV 3))

(test/exn (prueba-interp '{+ 1 #f}) "typeof: Error in parameter (boolS #f)\nExpected type: (numberT)\nGiven type: (booleanT)")

(test/exn (prueba-interp '{and 1 #f}) "typeof: Error in parameter (numS 1)\nExpected type: (booleanT)\nGiven type: (numberT)")

(test (prueba-interp '{if #t 2 3}) (numV 2))

(test/exn (prueba-interp '{if #t 2 #t}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test/exn (prueba-interp '{if 3 2 #t}) "typeof: Type error\nConditional's test-expr must be a boolean\nGiven: (numberT)")

(test (prueba-interp '{with {{x : number 2} {y : boolean #t} {z : number 1}} {if y x z}}) (numV 2))

(test/exn (prueba-interp '{cond {#t 2} {#f 3} {else #t}}) "typeof: Type error\nconditionals must have same type in then-expr and else-expr")

(test (prueba-interp '{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}}) (closure '(x y) (iF (id 'y) (id 'x) (num 0)) (mtSub)))

(test/exn (prueba-interp '{fun {{x : number} {y : number}} : (number number -> number) {if y x 0}}) "typeof: Type error\nConditional's test-expr must be a boolean\nGiven: (numberT)")

(test (prueba-interp '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 #t}}) (numV 2))

(test/exn (prueba-interp '{{fun {{x : number} {y : boolean}} : (number boolean -> number) {if y x 0}} {2 3}})
          "typeof: Type error:\nParameter's type doesn't match expected types\nGiven: (numberT)\nExpected: (booleanT)")
