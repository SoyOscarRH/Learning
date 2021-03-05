#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE

;;   (type-case WAE expr
;;   [id    (i)]
;;   [num   (n)]
;;   [op    (f) (args) ]
;;   [with  (bindings) (body)]
;;   [with* (bindings) (body)]
;;              )
(define (subst expr sub-id value)
  (type-case WAE expr
             [id (i) (if (symbol=? i sub-id) value expr)]
             [num (n) expr]
             [op (f args) (op f (map (lambda (arg)
                                    (subst arg sub-id value))
                                  args))]
             [with (bi bo) (with (map (lambda (bound)
                                        (binding (binding-id bound)
                                                 (subst (binding-value bound) sub-id value))) bi)
                                 (if (member sub-id (map binding-id bi))
                                     bo
                                     (subst bo sub-id value)))]
             [with* (bi bo) (error "subst: With* encontrado")]))
         
;; Toma un árbol de sintáxis abstraca del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number

;;   (type-case WAE expr
;;   [id    (i)]
;;   [num   (n)]
;;   [op    (f) (args) ]
;;   [with  (bindings) (body)]
;;   [with* (bindings) (body)]
;;              )

(define (interp expr)
  (type-case WAE expr
             [id (i) (error (format  "interp: Variable libre: '~a" i))]
             [num(n) n]
             [op (f args) (apply f   (map (λ  (arg) (interp arg ) )args))]
             [with (bi bo)(interp (foldr (λ (bi e)
                                           (subst e (binding-id bi) (num (interp (binding-value bi)))))
                                         bo bi))]
             [with* (bi bo) (error "interp: With* received sugared expr")]
             ))

 

