#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el tipo de una expresión en el contexto de tipos
(define (type-lookup name tc)
  (type-case Type-Context tc
             (phi () (error (format "type: no type information for: ~a" name)))
             (gamma (id type tc-rest)
                    (if (symbol=? name id)
                        type
                        (type-lookup name tc-rest)))))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof SCFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
             [idS (i) (type-lookup i context)]
             [numS (n) (numberT)]
             [boolS (b) (booleanT)]
             [opS (f args) (typeof-op f args context)]
             [iFS (Cond Then Else) (typeof-if Cond Then Else context)]
             [withS (bi bo) (typeof-with bi bo context)]
             [withS* (bi bo) (typeof-with* bi bo context)]
             [condS (cases) (typeof-cases cases context)]
             [funS (param rType bo) (typeof-fun param rType bo context)]
             [appS (fun args) (typeof-app fun args context)]
))

(define (typeof-app fun args context)
  ;; Check arguments
  (let ([fun-arg-types (funT-params (funS-rType fun))])
    (if (equal? (take fun-arg-types (sub1 (length fun-arg-types)))
                (map (lambda (v) (typeof v context)) args))
        (last (funT-params (typeof fun context)))
        (let-values ([(x y) (drop-common-prefix
                          (map (lambda (v) (typeof v context)) args)
                          (take fun-arg-types (sub1 (length fun-arg-types))))])
        (error (format "typeof: Type error:\nParameter's type doesn't match expected types\nGiven: ~v\nExpected: ~v" (first x) (first y)))
          ))))

(define (typeof-fun param rType bo context)
  ;; Check argument signature
  (let ([signature-types (funT-params rType)])
    (if (equal? (map param-tipo param) (take signature-types (sub1 (length signature-types))))
        ;; Check body signature
        (let* ([new-context (foldr (lambda (x y v) (gamma x y v)) context
                                   (map param-param param)
                                   (map param-tipo param))]
               [body-type (typeof bo new-context)])
          (if (equal? body-type (last (funT-params rType)))
              rType
              (error (format "Type: Expected function type does not match actual type"))))
        (error (format "Type: Function arguments do not match signature")))))

(define (typeof-cases cases context)
  ;; Check cond types
  (if (andmap (lambda (v) (when (condition? v) (booleanT? (typeof (condition-test-expr v) context)))) cases)
      (let ([else-type (typeof (else-cond-else-expr (last cases)) context)]
            [normal-cases (take  cases (sub1 (length cases)))])
        (if (andmap (lambda (v) (equal?
                                 (typeof (condition-then-expr v) context)
                                 else-type)) normal-cases )
            else-type
            (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr")))
      (error (format "typeof: Type error\nConditional's test-expr must be a boolean"))))

(define (typeof-with bi bo context)
  (let ([extended-context (foldr (lambda (x v) (if (equal? (typeof (binding-value x) context) (binding-tipo x))
                           (gamma (binding-id x) (binding-tipo x) v)
                           (error "Type: With type does not match")))
                                 context bi)])
    (typeof bo extended-context)))

(define (typeof-with* bi bo context)
  (let ([extended-context (foldr (lambda (x v) (if (equal? (typeof (binding-value x) v) (binding-tipo x))
                           (gamma (binding-id x) (binding-tipo x) v)
                           (error "Type: With type does not match")))
                                 context bi)])
    (typeof bo extended-context)))

;; Toma un operador y verifica que el tipo de los argumentos corresponda con el tipo del operador
;; Por el momento, se asume que los operadores distintos de and, or, not requieren tipos numéricos
(define (typeof-op f args context)
  (let* ([mismatch (lambda (p) (first (filter-map (lambda (v) (and (not (p (typeof v context))) v)) args)))]
         [mismatch-type (lambda (p) (typeof (mismatch p) context))])
  (cond
    [(member (object-name f) '(and or not))
             (if (andmap (lambda (v) (booleanT? (typeof v context))) args)
                 (booleanT)
                 (error (format "typeof: Error in parameter ~v\nExpected type: (booleanT)\nGiven type: ~v"
                                (mismatch booleanT?) (mismatch-type booleanT?)))
                 )]
    [(member (object-name f) '(<= < = > >=))
             (if (andmap (lambda (v) (numberT? (typeof v context))) args)
                 (booleanT)
                 (error (format "typeof: Error in parameter ~v\nExpected type: (numberT)\nGiven type: ~v"
                                (mismatch numberT?) (mismatch-type numberT?)))
                 )]
    [else (if (andmap (lambda (v) (numberT? (typeof v context))) args)
                 (numberT)
                 (error (format "typeof: Error in parameter ~v\nExpected type: (numberT)\nGiven type: ~v"
                                (mismatch numberT?) (mismatch-type numberT?))))])))

(define (typeof-if Cond Then Else context)
  (if (booleanT? (typeof Cond context))
      (let ([type-then (typeof Then context)])
        (if (equal? type-then (typeof Else context))
            type-then
            (error (format "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))))
      (error (format "typeof: Type error\nConditional's test-expr must be a boolean\nGiven: ~v" (typeof Cond context)))))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))
