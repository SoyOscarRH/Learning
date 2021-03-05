#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
      (let
        ([parse-binding (λ (b) (binding (first b) (parse (second b))))])
        (case (first sexp)
          ;; opetations
          [(+)      (op +       (map parse (rest sexp)))]
          [(-)      (op -       (map parse (rest sexp)))]
          [(*)      (op *       (map parse (rest sexp)))]
          [(/)      (op /       (map parse (rest sexp)))]

          [(modulo) (safe-op modulo sexp)]
          [(expt)   (safe-op expt sexp)]
          [(add1)   (safe-op add1 sexp)]
          [(sub1)   (safe-op sub1 sexp)]

          ;; operations
          [(with)   (safe-with sexp parse-binding)]
          [(with*)  (desugar-with (with* (map parse-binding (second sexp)) (parse (third sexp))))]
          ))]))

(define (desugar-with expr)
  (foldr (lambda (v l)
           (with (list v) l)) (with*-body expr) (with*-bindings expr)))

(define (safe-with sexp parse-binding)
  (let
      ([duplicates (check-duplicates (flatten (map (lambda (x) (first x)) (second sexp))))])
    (if duplicates
        (error (format "parse: El símbolo '~a está declarado más de una vez" duplicates))
        (with  (map parse-binding (second sexp)) (parse (third sexp)))
      )))

(define (safe-op func sexp)
  (if (= (procedure-arity func) (sub1 (length sexp)))
      (op func (map parse (rest sexp)))
      (error (format "parser: Aridad incorrecta en la función ~v" (first sexp)))))
