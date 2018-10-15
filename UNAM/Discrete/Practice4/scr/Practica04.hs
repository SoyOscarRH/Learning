--Estructuras Discretas 2019-1
--Profesor: Manuel Alcántara Juárez
--Ayudante: Rafael de Jesús García García
--Práctica 4
--Fecha de entrega: 03 de octubre de 2018

module Main where

    -- Recibe un número y devuelve True si es par o False e.o.c
    -- valor 1 pt
    par :: Int -> Bool
    par x = mod x 2 == 0
    
    -- Recibe un número y devuelve True si es impar o False e.o.c
    -- valor 1 pt
    impar :: Int -> Bool
    impar x = not (par x)
    
    -- Función fst para tuplas de 3 elementos
    -- valor 1 pt
    fstN :: (a, b, c) -> a
    fstN (a, b, c) = a
    
    -- Función snd para tuplas de 3 elementos
    -- valor 1 pt
    sndN :: (a, b, c) -> b
    sndN (a, b, c) = b
    
    -- Función trd para tuplas de 3 elementos
    -- valor 1 pt
    trdN :: (a, b, c) -> c
    trdN (a, b, c) = c
    
    -- Suma el primer elemento (cabeza) de una lista con el último elemento
    -- valor 1 pt
    sumaElem :: [Int] -> Int
    sumaElem [] = 0
    sumaElem list = head (list) + last(list)
    
    -- Toma los primeros 5 elementos de una lista y devuelve la reversa de esa lista de
    -- 5 elementos. Si la lista tiene menos de 5 elementos devuelve una lista vacía
    -- valor 1 pt
    reversa5 :: [Int] -> [Int]
    reversa5 x = 
        if length x < 5 then []
        else reverse (take 5 x)

    -- Recibe un entero y devuelve la suma de sus dígitos
    -- valor 2 pts
    sumaDigitos :: Int -> Int
    sumaDigitos 0 = 0
    sumaDigitos x = do 
        if x > 0 then (x `mod` 10) + sumaDigitos (x `div` 10) else sumaDigitos(x * (-1) )
    
    -- Recibe una lista de enteros.
    -- Duplica el elemento que se encuentre cada dos posiciones empezando por el final.
    -- Es decir, el primer elemento a duplicar es el último, luego el de la posición
    -- n - 2, luego n - 4 ...
    -- valor 2 pts.

    takeReves :: Int -> [a] -> [a]
    takeReves n xs = if n < 0 then [] else let m = length xs in drop (m-n) xs

    delete2 :: [Int] -> Int
    delete2 x = length x - 2

    duplicaCadaDos :: [Int] -> [Int]
    duplicaCadaDos [] = []
    duplicaCadaDos [a] = [a*2]
    duplicaCadaDos [a, b] = [a, b*2]
    duplicaCadaDos x = do
        let size = length x
        let allBut1 = take (size - 1) x 
        let allBut2 = take (size - 2) x 
        duplicaCadaDos allBut2 ++ [last(allBut1)] ++ [last(x)*2]
    
    main = do
            
        -- Debe devolver True
        let prueba1 = par 10
    
        -- Debe devolver True
        let prueba2 = par (-2)
    
        -- Debe devolver False
        let prueba3 = par 3
    
        -- Debe devolver False
        let prueba4 = impar 2
    
        -- Debe devolver False
        let prueba5 = impar (-2)
    
        -- Debe devolver True
        let prueba6 = impar 3
    
        -- Debe devolver 'l'
        let prueba7 = fstN ('l', 'o', 'l')
    
        -- Debe devolver 1
        let prueba8 = sndN (6, 1, 9)
    
        -- Debe devolver [True,False]
        let prueba9 = trdN (1.5, "glhf", [True, False])
    
        -- Debe devolver 0
        let prueba10 = sumaElem []
    
        -- Debe devolver 10
        let prueba11 = sumaElem [5]
    
        -- Debe devolver 12
        let prueba12 = sumaElem [2,4,6,8,10]
    
        -- Debe devolver []
        let prueba13 = reversa5 [1,2,3]
    
        -- Debe devolver [10,8,6,4,2]
        let prueba14 = reversa5 [n | n <- [1..100], (n `mod` 2) == 0]
    
        -- Debe devolver 17
        let prueba15 = sumaDigitos 15623
    
        -- Debe devolver 2
        let prueba16 = sumaDigitos (-20)
    
        -- Debe devolver [2,2,6,4]
        let prueba17 = duplicaCadaDos [1,2,3,4]
    
        -- Debe devolver [3,14,5,16,2]
        let prueba18 = duplicaCadaDos [3,7,5,8,2]
    
        -- Debe devolver []
        let prueba19 = duplicaCadaDos []
    
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
        print prueba14
        print prueba15
        print prueba16
        print prueba17
        print prueba18
        print prueba19
    


        -- ESTA MAL LOS ULTIMOS DOS CASOS DE PRUEBA BASADO EN LA DEFINICION DE TU FUNCION
        -- NO DEBERIAN REGRESAR ESTO