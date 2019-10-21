;;;============================================================================================================
;;;  GATO4X4
;;;      Este es un ejemplo del uso de los elementos que debe incluir el agente jugador en
;;;		 su código.
;;;
;;;      Función tictactoe: 
;;;         Debe existir una función llamada tictactoe la cuál recibirá un tablero 4x4 que representa 
;;;			el juego. El argumento de la función entonces, es una lista que contiene los elementos 
;;;			del tablero actual.
;;;                 ejemplo de argumento a recibir:
;;;						(( NIL NIL NIL NIL)
;;;						 ( NIL NIL  X  NIL)
;;;						 ( NIL NIL NIL NIL)
;;;						 ( NIL NIL NIL NIL))
;;;						-los elementos de la lista principal representan los renglones
;;;                  	-las posiciones de los elementos de las sublistas representan las columnas
;;;						-los elementos de cada sublista representan el estado de cada casilla
;;;								*NIL - significa que la casilla se encuentra vacía
;;;								*X o O - significa que la casilla está ocupada, X corresponde a
;;;									     a la jugada de una persona y O a la jugada del agente jugador
;;;      Varible *output*:
;;;			El agente jugador debe entregar como respuesta un número del 1 al 16 (la posición en que
;;;			tirará su agente jugador), estos números representan las posiciones de casillas en 
;;;			el tablero y esta respuesta debe ser guardada en la variable *output*, no debe declarar
;;;			esta variable, sólo haga uso de ella.
;;;
;;;		Su agente recibirá el estado actual del tablero en cada jugada.
;;;============================================================================================================

;;; 		El siguiente código es un ejemplo de cómo hacer uso de la función tictactoe y la
;;;			variable *output*

(defparameter num 0)

;;;	En este caso la función tictactoe recibe el tablero y lo recorre hasta encontrar una casilla 
;;; vacía y guarda el número de posición de la casilla en la variable *output*.
(defun tictactoe (tablero)
	(loop named res for x in tablero do
	       (loop for y in x do
		   		(incf num)
		    	if (null y) do
									(setq *output* num) 
		    				  (return-from res num) )))

;;;En la pestaña debug siempre se imprimirá el tablero y la variable *output*
(print "Esta es una prueba para la pestaña debug")

