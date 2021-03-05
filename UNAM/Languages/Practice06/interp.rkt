#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error (format "lookup: Variable libre: ~a" name))]
    [aSub (n v dss)
          (if (symbol=? name n)
              v
              (lookup name dss))]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
             [id (i) (lookup i ds)]
             [num (n) (numV n)]
             [bool (b) (boolV b)]
             [op (f args) (enclose-type (apply f (map (lambda (arg) (extract-type (interp arg ds))) args)))]
             [iF (i t e) (if-impl i t e ds)]
             [fun (p b) (closure p b ds)]
             [app (f a) (let*
                            ([fun-val (interp f ds)]
                             [fun-env (closure-env fun-val)]
                             )
                          (interp (closure-body fun-val)
                                  (foldr (lambda (param arg subs) (aSub param arg subs))
                                         fun-env
                                         (closure-param fun-val)
                                         (map (lambda (arg) (interp arg ds)) a)))
                          )]))

(define (if-impl i t e ds)
  (let ([if-value (extract-type-message (interp i ds)
                                 "interp: Símbolo no esperado la condicional de if, no es un booleano")])
    (if if-value
        (interp t ds)
        (interp e ds))))


(define (extract-type-message expr message)
  (type-case CFWBAE-Value expr
             [numV (n) n]
             [boolV (b) b]
             [else (error message)]))


(define (extract-type expr)
  (type-case CFWBAE-Value expr
             [numV (n) n]
             [boolV (b) b]
             [else (error (format "interp: ~a no es primitiva" expr))]))

(define (enclose-type val)
  (cond
    [(number? val)(numV val)]
    [(boolean? val)(boolV val)]
    [else (error (format "interp ~a no es primitiva" val))]))
