#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr)
  (type-case SCFWBAE sexpr
             [idS    (i) (id i)]
             [numS   (n) (num n)]
             [boolS  (b) (bool b)]
             [iFS    (c t e) (iF (desugar c) (desugar t) (desugar e))]
             [opS    (f a) (op f (map desugar a))]
             [condS  (cases) (desugar-cond cases)]
             [withS  (bi bo) (desugar-with sexpr)]
             [withS* (bi bo) (desugar (desugar-with* sexpr))]
             [funS   (p b) (fun p (desugar b))]
             [appS   (f a) (app (desugar f) (map desugar a))]))

(define (desugar-with expr)
  (type-case SCFWBAE expr
             [withS (bi bo) (app
                             (fun (map binding-id bi) (desugar bo))
                             (map (compose desugar binding-value) bi))]
             [else expr]))

(define (desugar-with* expr)
  (type-case SCFWBAE expr
             [withS* (bi bo) (foldr (lambda (x b) (withS (list x) b)) bo bi)]
             [else expr]))

(define (desugar-cond cases)
  (foldr (lambda (x b) (iF (desugar (condition-test-expr x)) (desugar (condition-then-expr x)) b ))
         (desugar (else-cond-else-expr (last cases)))
         (take cases (sub1 (length cases)))))
(define expr (parse (quote (with ((x 1) (y 2)) (with* ((z 3) (w z)) (with ((f (fun (x) x))) (f (w))))))))
