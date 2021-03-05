#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
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

          [(if0) (if0 (parse (second sexp)) (parse (third sexp))  (parse (fourth sexp)))]

          ;; operations -- TODO: Desugar with into func
          [(with)   (desugar-with (safe-with sexp parse-binding))]
          [(with*)  (desugar-with (desugar-with-star (with* (map parse-binding (second sexp)) (parse (third sexp)))))]
          ;; function
          [(fun) (check-fun-duplicates sexp)]
          [else (check-fun-app-arity (app (parse (first sexp)) (map parse (flatten (rest sexp)))))]
          ))]))

(define (check-fun-app-arity expr)
  (type-case CFWAE expr
             [app (f args) (if (fun? f)
                               (let
                                   ([length-params (length (fun-params f))]
                                    [length-args (length args)])
                                 (if (equal? length-params length-args)
                                     expr
                                     (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función")))
                               expr)]
             [else expr]))

(define (desugar-with expr)
  (type-case CFWAE expr
             [with (bi bo) (app
                            (fun (map binding-id bi) (desugar-with bo))
                            (map binding-value bi))]
             [else expr]))

(define (desugar-with-star expr)
  (foldr (lambda (v l)
           (with (list v) l)) (with*-body expr) (with*-bindings expr)))

(define (check-fun-duplicates sexp)
  (let
      ([duplicates (check-duplicates (second sexp))])
    (if duplicates
        (error (format "parser: parámetro definido dos veces: ~a" duplicates))
        (fun (second sexp) (parse (third sexp))))))

(define (safe-with sexp parse-binding)
  (let
      ([duplicates (check-duplicates (flatten (map (lambda (x) (first x)) (second sexp))))])
    (if duplicates
        (error (format "parser: El símbolo '~a está declarado más de una vez" duplicates))
        (with  (map parse-binding (second sexp)) (parse (third sexp)))
      )))

(define (safe-op func sexp)
  (if (= (procedure-arity func) (sub1 (length sexp)))
      (op func (map parse (rest sexp)))
      (error (format "parser: Aridad incorrecta en la función ~v" (first sexp)))))
