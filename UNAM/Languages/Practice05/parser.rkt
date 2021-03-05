#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(eq? sexp 'true) (boolS #t)]
    [(eq? sexp 'false) (boolS #f)]
    [(symbol? sexp) (idS sexp)]
    [(list? sexp)
     (let
         ([parse-binding (λ (b) (binding (first b) (parse (second b))))])
       (case (first sexp)
         ;; operations
         [(+)      (safe-op + sexp)]
         [(-)      (safe-op - sexp)]
         [(*)      (safe-op * sexp)]
         [(/)      (safe-op / sexp)]
         [(modulo) (safe-op modulo sexp)]
         [(expt)   (safe-op expt sexp)]
         [(add1)   (safe-op add1 sexp)]
         [(sub1)   (safe-op sub1 sexp)]
         [(<)      (safe-op < sexp)]
         [(<=)     (safe-op <= sexp)]
         [(>)      (safe-op > sexp)]
         [(>=)     (safe-op >= sexp)]
         [(=)      (safe-op = sexp)]

         [(not)     (safe-op not sexp)]
         [(and)      (safe-op (lambda x (andmap identity x)) sexp)]
         [(or)      (safe-op (lambda x (ormap identity x)) sexp)]
         [(zero?)   (safe-op zero? sexp)]

         [(if)     (safe-if sexp)]
         [(cond)   (condS (parse-cond (rest sexp)))]

         [(with)   (safe-with sexp parse-binding)]
         [(with*)  (withS* (map parse-binding (second sexp)) (parse (third sexp)))]


          [(fun) (check-fun-duplicates sexp)]
          [else (check-fun-app-arity (appS (parse (first sexp)) (map parse (flatten (rest sexp)))))]

    ))]))

;; Asegura que el if contiene las expresiones adecuadas
(define (safe-if sexp)
  (if (eq? (length sexp) 4)
      (iFS (parse (second sexp)) (parse (third sexp))  (parse (fourth sexp)))
      (error "parser: Falta la else-expresion")))


;; Asegura que la aridad del operador corresponda al número de argumentos
(define (safe-op func sexp)
  (if (procedure-arity-includes? func (sub1 (length sexp)))
      (opS func (map parse (rest sexp)))
      (error (format "parser: Aridad incorrecta en la función ~v" (first sexp)))))

;; Asegura que el with no contenga bindings duplicados
(define (safe-with sexp parse-binding)
  (let
      ([duplicates (check-duplicates (flatten (map (lambda (x) (first x)) (second sexp))))])
    (if duplicates
        (error (format "parser: El símbolo '~a está declarado más de una vez" duplicates))
        (withS  (map parse-binding (second sexp)) (parse (third sexp)))
      )))

;; Asegura que la función no duplica identificadoes de parámetros
(define (check-fun-duplicates sexp)
  (let
      ([duplicates (check-duplicates (second sexp))])
    (if duplicates
        (error (format "parser: parámetro definido dos veces: ~a" duplicates))
        (funS (second sexp) (parse (third sexp))))))

;; Asegura que la aplicación de función recibe el número exacto de argumentos
;; para funciones anónimas
(define (check-fun-app-arity expr)
  (type-case SCFWBAE expr
             [appS (f args) (if (fun? f)
                               (let
                                   ([length-params (length (fun-params f))]
                                    [length-args (length args)])
                                 (if (equal? length-params length-args)
                                     expr
                                     (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función")))
                               expr)]
             [else expr]))

;; Toma una lista de parejas de condiciones y genera la sintáxis abstracta
;; de una condicional en CFWBAE
;; parse-cond: A -> SCFWBAE
;; parse-cond: s-expression -> SCFWBAE
(define (parse-cond cond-expr)
  (append
   (map (lambda (expr) (condition (parse (first expr)) (parse (second expr))))
       (take cond-expr (sub1 (length cond-expr))))
   (let ([expr (last cond-expr)])
     (if (eq? (first expr) 'else)
         (list (else-cond (parse (second expr))))
         (error (format "parser: Falta la else-expresion"))))))
