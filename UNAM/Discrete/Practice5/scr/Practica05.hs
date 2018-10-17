--Estructuras Discretas 2019-1
--Profesor: Manuel Alcántara Juárez
--Ayudante: Rafael de Jesús García García
--Práctica 5
--Oscar Andres Rosas Hernandez

module Main where

    ------------------------------
    --     Ejemplo producto     --
    prod :: Int -> Int -> Int
    prod 0 m = 0
    prod n 0 = 0
    prod n m = n + prod n (m - 1)
    ------------------------------

    -- Recibe dos enteros n y m y devuelve la potencia de n^m
    -- valor 1 pt
    eleva :: Int -> Int -> Int
    eleva 0 0 = error "Ahhh"
    eleva base 0 = 1
    eleva 0 exponent = 0
    eleva base exponent = prod base (eleva base (exponent - 1))

    -- Recibe un entero y calcula su factorial
    -- valor 1 pt
    fact :: Int -> Int
    fact 0 = 1
    fact n = n * fact(n-1)

    -- Recibe una lista de enteros y devuelve la suma de los elementos
    -- valor 2 pts
    sumaLista :: [Int] -> Int
    sumaLista [] = 0 
    sumaLista list = head(list) + sumaLista(tail(list))

    -- Recibe un elemento y una lista.
    -- Devuelve el número de veces que aparece ese elemento en la lista
    -- valor 2 pts
    cuentaElem :: (Eq a) => a -> [a] -> Int
    cuentaElem e [] = 0
    cuentaElem e x = do 
      if (head(x) == e) then 
        1 + cuentaElem e (tail x) 
      else 
        cuentaElem e (tail x)

    -- Recibe un elemento y una lista.
    -- Devuelve la lista sin todas las incidencias de ese elemento
    -- valor 2 pts
    eliminaElem :: (Eq a) => a -> [a] -> [a]
    eliminaElem e [] = []
    eliminaElem e x = do 
      if (head(x) == e) then 
        eliminaElem e (tail x) 
      else 
        [head(x)] ++ eliminaElem e (tail x)

    -- Recibe una lista.
    -- Devuelva una lista que indica cuantas veces aparece cada elemento de
    -- la lista original
    -- valor 2 pts
    repeticiones :: (Eq a) => [a] -> [(a,Int)]
    repeticiones [] = []
    repeticiones x = do
      let h = head x
      let rest = eliminaElem h x
      [(h, cuentaElem h x)] ++ repeticiones rest


    main = do

        --Debe regresar 262144
        let prueba1 = eleva 8 6

        -- Devuelve 3628800
        let prueba2 = fact 10

        --Debe regresar 414
        let prueba3 = sumaLista [45,78,12,89,134,56]

        --Debe regresar 4834
        let prueba4 = sumaLista [456,1234,789,2345,10]

        --Debe regresar 6
        let prueba5 = cuentaElem 7 [7,3,5,7,12,7,89,7,0,7,23,7]

        --Debe regresar 4
        let prueba6 = cuentaElem 34 [56,34,78,59,34,80,34,12,50,34]

        -- Debe regresar [1,4,2,2,1,4,1,7]
        let prueba7 = eliminaElem 3 [1,3,4,2,2,1,3,4,1,7]

        -- Debe regresar [7,3,5,7,12,7,89,7,0,7,23,7]
        let prueba8 = eliminaElem 1 [7,3,5,7,12,7,89,7,0,7,23,7]

        -- Debe regresar [(5,3),(7,3),(4,2),(3,1),(8,2),(1,2)]
        let prueba9 = repeticiones [5,7,5,4,3,4,7,7,8,5,8,1,1]

        --Debe regresar [(45,4),(67,3),(23,1),(89,2),(24,1),(56,1),(90,2),(78,1),(34,1),(12,1)]
        let prueba10 = repeticiones [45,45,67,23,67,89,24,56,90,78,45,34,12,67,89,90,45]

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