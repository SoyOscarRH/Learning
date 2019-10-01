;;;Código de ejemplo.
;;;Este código muestra como deben usarse las funciones de la biblioteca
;;;maze-lib.lsp para que pueda ser usado en la página de internet. Este código
;;;sólo funciona para el laberinto Chico, ya que la solución ya está escrita
;;;dentro del código. Si se usa en los demás laberintos surgirán mensajes de
;;;error.

;Primero debe cargarse la biblioteca de la siguiente forma.
(load "maze_lib.lisp")

;Para añadir un algoritmo al menú de la página es necesario usar la función
;add-algorithm, como se muestra a continuación. No importa en que lugar
;del archivo se use, pero de preferencia que sea al inicio del código.
;Si están haciendo pruebas en su computadora entonces no hay problema si se
;omiten.
(add-algorithm 'breadth-first)
(add-algorithm 'depth-first)
(add-algorithm 'error-example)

;Función de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;ancho. Esta función no debe llevar argumentos.
(defun breadth-first ()
  ;La posición inicial y la posición meta del laberinto están almacenadas en las
  ;variables *start* y *goal*. Estas son arreglos de dos elementos, el
  ;primero es la fila y la segunda la columna. La posición #(0 0) está ubicada en
  ;la celda superior izquierda. La primer coordenada es el número de fila (y) y
  ;la segunda es el número de columna (x).
  (format t "Posición inicial: ~S~%" *start*) (format t "Posición meta: ~S~%" *goal*)
  ;Para saber cuantas filas y cuantas columnas tiene el laberinto en total, es
  ;necesario usar las funciones get-maze-rows y get-maze-cols
  (format t "Número total de filas: ~S~%" (get-maze-rows))
  (format t "Número total de columnas ~S~%" (get-maze-cols))
  ;Para pedir las paredes de una celda determinada es necesario usar la función
  ;get-cell-walls. Recibe dos argumentos: el número de la fila y el número de la
  ;columna.
  (format t "Paredes de la celda #(0 3): ~S~%" (get-cell-walls 0 3))
  ;Si sienten la necesidad de ver dibujadas las paredes de una celda, pueden
  ;usar la función draw-cell-walls.
  (format t "Dibujo de las paredes de la celda #(0 3):~%")
  (draw-cell-walls 0 3)
  ;Si desean obtener la información de todas las paredes del laberinto pueden
  ;usar la función get-maze-data
  (format t "Datos del laberinto: ~%~S~%" (get-maze-data))
  ;La solución debe almacenarse en la variable global *solution*, la cual ya
  ;está declarada dentro de la biblioteca maze_lib.
  (setq *solution* '(3 4 4 4 7 0 7 7 5 3 3 5 7)))
  ;La solución debe ser expresada como una lista conteniendo las direcciones de
  ;los desplazamientos. Cada dirección está representada por un número, empezando
  ;desde arriba con el 0 y después en sentido horario. De esta forma, los 8
  ;movimientos posibles son:
  ;Arriba (N): 0
  ;Arriba-derecha (NE): 1
  ;Derecha (E): 2
  ;Abajo-derecha (SE): 3
  ;Abajo (S): 4
  ;Abajo-izquierda (SW): 5
  ;Izquierda (W): 6
  ;Arriba-izquierda (NW): 7


;Función de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;profundo. Esta función no debe llevar argumentos.
(defun depth-first ()
 (setq *solution* '(3 4 4 5 0 0 7 7 5 3 3 6 5 0)))

;Función defectuosa. Esta función genera un error al trater de obtener las
;paredes de una celda fuera de las fronteras del laberinto. Esto es para que
;puedan ver como se despliegan los errores de ejecución dentro de la página.
(defun error-example () (get-cell-walls 1000 1000))

;La última línea ejecutable del código debe ser la siguiente. Es la que se
;encarga de enviar la solución a la página de internet para que pueda ser
;dibujada. Si están haciendo pruebas en su computadora entonces pueden omitirla
;o comentarla.
(start-maze)
