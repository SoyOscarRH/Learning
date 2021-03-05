#lang plai
;; Ejercicio 1
;; Función que recibe n, r y devuelve el conjunto con los primeros r múltiplos de n.
;; multiplos: number number -> (listof number)
(define (multiplos n r)
  (for/list ([i r]) 
    (* (add1 i) n)))

;; Ejercicio 2
;; Predicado que nos dice si un número m es divisor de otro número n.
;; Si el parámetro recibido es cero, se devuelve un error.
;; divisor?: number number -> number
;; (quotient n m) es equivalente a: (truncate (/ n m))
(define (divisor? m n)
  (if (= 0 m)
      (error "Error: el cero no es divisor de nadie")
      (let ([rem (- n (* m (quotient n m)))])
        (= 0 rem))))

;; Ejercicio 3
;; Función que nos da el una lista de divisores de un número pasado como parámetro
;; divisores: number -> (listof number)
(define (divisores n)
  (if (= 0 n)
      (error "Error: Los divisores de cero son todos los enteros distintos a cero")
      (filter (λ(x) (divisor? x n)) (for/list ([i (in-range n)]) (+ 1 i)))))

;; Ejercicio 4
;; Función que recibe un elemento a, una lista l y decide si a pertenece a l.
;; pertenece: a (listof a) -> boolean
(define (pertenece? a l)
  (cond
    [(empty? l) false]
    [(equal? (first l) a) true]
    [else (pertenece? a (rest l))]))

;; Ejercicio 5
;; Función que recibe una lista l con elementos. Devuelve una lista sin repeticiones con los elementos de l.
;; eliminaRep: (listof a) -> (listof a)
(define (eliminaRepetidos lista)
  (if (empty? lista)
      lista
      (cons (car lista) (filter (λ (x) (not (equal? x (car lista)))) (eliminaRepetidos (cdr lista))))))

;; Estructura que nos permite modelar puntos en el plano.
;; Sirve para modelar figuras geométricas.
(struct Punto (x y) #:inspector #f)

;; Ejercicio 6
;; Funcion que nos permite calcular el punto equidistante entre dos puntos.
;; Si alguno de los dos parámetros recibidos no es un punto, lanza un error
;; punto-medio: Punto Punto -> Punto
(define (punto-medio p q)
   (if (or (not (Punto? p)) (not (Punto? q)))
      (error "Algo anda mal")
      (let ([x1 (Punto-x p)]
            [y1 (Punto-y p)]
            [x2 (Punto-x q)]
            [y2 (Punto-y q)])
      (Punto (/ (+ x1 x2) 2) (/(+ y1 y2) 2)))
      )
  )

;; Ejercicio 7
;; Funcion que nos permite calcular la distancia entre dos puntos.
;; Si alguno de los dos parámetros recibidos no es un punto, lanza un error
;; distancia: Punto Punto -> number
(define (distancia p q)
  (if (or (not (Punto? p)) (not (Punto? q))) 
    (error "input should be points")
    (let (
      [x1 (Punto-x p)] [x2 (Punto-x q)]
      [y1 (Punto-y p)] [y2 (Punto-y q)]
      [square (λ (x) (* x x))])  
      
        (sqrt 
          (+ (square (- x2 x1)) (square (- y2 y1)))))))

;; Ejercicio 8
;; Definición del tipo abstracto de datos Figura
(define-type Figura
  [Circulo (centro Punto?) (radio number?)]
  [Triangulo (p1 Punto?) (p2 Punto?) (p3 Punto?)]
  [Cuadrado (esquinaSubIzquierda Punto?) (ancho number?)]
  [Rectangulo (esquinaSubIzquierda Punto?) (ancho number?) (altura number?)])

;; Ejercicio 9
;; Función que recibe una figura y calcula su perímetro.
;; perimetro: Figura -> number
(define (perimetro fig)
  (type-case Figura fig
    [Circulo (_ radio)(* (* 2 pi) radio)]
    [Cuadrado (_ ancho) (+ ancho ancho ancho ancho )]
    [Rectangulo (_ ancho altura )(* 2(+ ancho altura ))]
    [Triangulo (p1 p2 p3 )
          (let ( [x1 (Punto-x p1)] [x2 (Punto-x p2)] [x3 (Punto-x p3)]
        [y1 (Punto-y p1)] [y2 (Punto-y p2)] [y3 (Punto-y p3)])
            (let ([ab (sqrt (+(*(- x2 x1)(- x2 x1)) (*(- y2 y1)(- y2 y1)))  )]
                  [ac (sqrt (+(*(- x3 x1)(- x3 x1)) (*(- y3 y1)(- y3 y1)))  )]
                  [bc (sqrt (+(*(- x3 x2 )(- x3  x2 )) (*(- y3 y2)(- y3 y2)))  )])

            (+(+ ab ac) bc)
      ))])) 

;; Ejercicio 10
;; Función que recibe una figura y calcula su área.
;; area: Figura -> number
(define (area fig) 
  (type-case Figura fig
    [Circulo (_ radio) (* pi radio radio)]
    [Cuadrado (_ ancho) (* ancho ancho)]
    [Rectangulo (_ ancho altura)  (* ancho altura)]
    [Triangulo (p1 p2 p3)  
      (let (
        [x1 (Punto-x p1)] [x2 (Punto-x p2)] [x3 (Punto-x p3)]
        [y1 (Punto-y p1)] [y2 (Punto-y p2)] [y3 (Punto-y p3)])
        (abs 
          (/
            (+
              (+ (* x1 y2)) (+ (* x2 y3)) (+ (* x3 y1))
              (- (* x1 y3)) (- (* x2 y1)) (- (* x3 y2)))
          2)))
    ]))

;; Punto extra
;; Función que nos da el elemento más repetido en una lista. 
;; Si hay dos o más elementos repetidos el mismo número de veces, devuelve el primero en aparecer de izquierda a derecha.
;; masRepetido (listof a) -> number
(define (masRepetido lista)
  (if (empty? lista)
      (error "La lista no debe ser vacía")
      (let ([apperances (make-hash)])
        (for ([element lista]) (hash-update! apperances element add1 0))
        (car (argmax cdr (hash->list apperances))))))
