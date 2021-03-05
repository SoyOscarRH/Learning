#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error (format "lookup: Hay un identificador libre: ~a" name))]
    [aSub (n v dss)
          (if (symbol=? name n)
              v
              (lookup name dss))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> CFWAE-Value
(define (interp expr ds)
  (type-case CFWAE expr
             [id (i) (lookup i ds)]
             [num (n) (numV n)]
             [op (f args) (numV (apply f (map (lambda (arg) (numV-n (interp arg ds))) args)))]
             [if0 (i t e) (if0-impl i t e ds)]
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
                          )]
             ;; Las siguientes deben haber sido sustituidas
             [with (bi bo) (error "interp: With encontrado")]
             [with* (bi bo) (error "interp: With* encontrado")]))


(define (safe-numV-n val)
  (if (numV? val)
      (numV-n val)
      (error "interp: Símbolo no esperado la condicional de if0, no es un número")))

(define (if0-impl i t e ds)
  (let ([if-value (safe-numV-n (interp i ds))])
    (if (equal? if-value 0)
        (interp t ds)
        (interp e ds))))
