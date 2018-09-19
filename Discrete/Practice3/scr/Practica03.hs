--Estructuras Discretas 2019-1
--Profesor: Manuel Alcántara Juárez
--Ayudante: Rafael de Jesús García García
--Práctica 3
--Nombre: Oscar Andres Rosas Hernandez 
--Fecha de entrega: Miércoles 19 de septiembre

module Main where

-- Recibe el radio de un círculo y calcula su área
-- valor: 1 pt.
areaCirculo :: (Floating a) => a -> a
areaCirculo radio = pi * radio * radio

-- Recibe la base y la altura de un triángulo y calcula su área.
-- valor: 1 pt.
areaTri :: (Floating a) => a -> a -> a
areaTri base altura = base * altura / 2

-- Recibe un ángulo en radianes y devuelve la secante de ese ángulo
-- valor: 1 pt.
secante :: (Floating a) => a -> a
secante angulo = 1 / cos angulo

-- Recibe dos coordenadas de puntos y calcula la distancia euclidiana entre ellos
-- valor: 1 pt.
distancia :: (Floating a) => (a,a) -> (a,a) -> a
distancia (x1, y1) (x2, y2) = sqrt ((dx * dx) + (dy * dy))
  where dx = x2 - x1
        dy = y2 - y1

-- Recibe un ángulo en radianes y determina su tipo
-- Los tipos que debe reconocer son: "agudo", "recto" y "obtuso"
-- Si no es de alguno de esos tipos devolver "otro"
-- tip: usar guardias
-- valor: 1 pt.
tipoAngulo :: (Floating a, Eq a, Ord a) => a -> String
tipoAngulo angulo
    | angulo < pi / 2   = "agudo"
    | angulo == pi / 2  = "recto"
    | angulo < pi       = "obtuso"
    | otherwise         = "otro"

--Realiza la operación lógica si y sólo si
-- valor: 1 pt.
syss :: Bool -> Bool -> Bool
syss False False = True
syss False True  = False
syss True  False = False
syss True  True  = True

--Realiza la operación lógica xor
-- valor: 1 pt.
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True  = True
xor True  False = True
xor True  True  = False


--Nos dice si los tres numeros son iguales
-- valor: 1 pt.
igualTres :: (Num a, Ord a) => a -> a -> a -> Bool
igualTres x y z = if (x == y && y == z) then True else False

--La primera entrada de cada tripleta se suma
--La segunda entrada de cada tripleta se multiplica
--La tercera entrada de cada tripleta se resta
-- valor: 3 pts.
-- tip: leer sobre las funciones show y read
opTrip :: (String, Int, String) -> (String,String,Int) -> (Int,String,String) -> (String,String,String)
opTrip (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = 
    (
        show (read x1 + read x2 + x3), 
        show (y1 * read y2 * read y3),
        show (read z1 - z2 - read z3)
    )

{-- Pruebas --}

main = do
    --Debe regresar 63.61725123519331
    let prueba1 = areaCirculo 4.5

    --Debe regresar 54.6
    let prueba2 = areaTri 21 5.2

    --Debe regresar 1.414213562373095
    let prueba3 = secante (pi / 4)

    --Debe regresar 16.1245154965971
    let prueba4 = distancia (5,25) (3,9)

    --Debe regresar "agudo"
    let prueba5 = tipoAngulo (pi / 4)

    --Debe regresar "recto"
    let prueba6 = tipoAngulo (pi / 2)

    --Debe regresar "obtuso"
    let prueba7 = tipoAngulo (3 * pi / 4)

    -- Debe regresar "otro"
    let prueba8 = tipoAngulo (2 * pi)

    --Debe regresar True
    let prueba9 = syss False False

    --Debe regresar True
    let prueba10 = xor True False

    --Debe regresar False
    let prueba11 = igualTres 3 4 5

    --Debe regresar True
    let prueba12 = igualTres 6 6 6

    --Debe regresar ("15","432","-15")
    let prueba13 = opTrip ("3",9,"5") ("7","6",7) (5,"8","13")

    print prueba1
    print prueba2
    print prueba3
    print prueba4
    print prueba5
    print prueba6
    print prueba7
    print prueba8
    print prueba9
    print prueba10
    print prueba11
    print prueba12
    print prueba13
