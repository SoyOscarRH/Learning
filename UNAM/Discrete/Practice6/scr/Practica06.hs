--Estructuras Discretas 2019-1
--Profesor: Manuel Alcántara Juárez
--Ayudante: Rafael de Jesús García García
--Práctica 6
--Oscar Andres Rosas Hernandez

module Main where

  -- Tipo de datos que representa a los números naturales
  data Natural = Cero | Suc Natural

  -- Instancia de Show
  instance Show Natural where
    show (Cero) = "0"
    show (Suc Cero) = "S 0"
    show (Suc n) = "S(" ++ show n ++ ")"

  --Suma de dos naturales
  -- valor: 1 pt
  suma :: Natural -> Natural -> Natural
  suma Cero n = n
  suma n Cero = n
  suma (Suc n) m = suma n (Suc m)

  --Producto de dos naturales
  -- valor: 1 pt
  prod :: Natural -> Natural -> Natural
  prod Cero n = Cero
  prod n Cero = Cero
  prod (Suc Cero) n = n
  prod n (Suc Cero) = n
  prod (Suc n) m = suma m (prod m n)

  --Igualdad de naturales
  -- valor: 1 pt
  igual :: Natural -> Natural -> Bool
  igual Cero Cero = True
  igual Cero n = False
  igual n Cero = False
  igual (Suc n) (Suc m) = igual n m

  --Nos indica si el natural de la izquierda es mayor que el de la derecha
  -- valor: 1 pt
  mayorQue :: Natural -> Natural -> Bool
  mayorQue Cero Cero = False
  mayorQue Cero n = False
  mayorQue n Cero = True
  mayorQue (Suc n) (Suc m) = mayorQue n m

  --Nos indica si el natural de la izquierda es menor que el de la derecha
  -- valor: 1 pt
  menorQue :: Natural -> Natural -> Bool
  menorQue Cero Cero = False
  menorQue Cero n = True
  menorQue n Cero = False
  menorQue (Suc n) (Suc m) = menorQue n m

  --Convertir de Natural a Int
  -- valor: 1 pt
  natToInt :: Natural -> Int
  natToInt Cero = 0
  natToInt (Suc n) = (natToInt n) + 1

  --Convertir de Int a Natural
  -- valor: 1 pt
  intToNat :: Int -> Natural
  intToNat 0 = Cero
  intToNat n = Suc (intToNat $ n - 1 )

  --Longitud de una lista expresada con el tipo Natural
  -- valor: 1 pt
  longNat :: [a] -> Natural
  longNat [] = Cero
  longNat x = suma (Suc Cero) (longNat (tail x)) 

  --Factorial de naturales
  -- valor: 1 pt
  factNat :: Natural -> Natural
  factNat Cero = Suc Cero
  factNat (Suc n) = prod (Suc n) (factNat n)

  --Fibonacci de naturales
  -- Definición: https://es.wikipedia.org/wiki/Sucesi%C3%B3n_de_Fibonacci#Definici%C3%B3n_recursiva
  -- valor: 1 pt
  fibNat :: Natural -> Natural
  fibNat Cero = Cero
  fibNat (Suc Cero) = Suc Cero
  fibNat (Suc (Suc n)) = suma (fibNat (Suc n)) (fibNat n)

  main = do

    --Debe regresar S(S(S(S(S(S(S 0)))))))
    let prueba1 = suma (intToNat 3) (intToNat 4)

    --Debe regresar S(S(S(S(S(S(S(S(S(S(S(S 0))))))))))))
    let prueba2 = prod (intToNat 3) (intToNat 4)

    --Debe regresar False
    let prueba3 = igual (intToNat 3) (intToNat 5)

    --Debe regresar False
    let prueba4 = mayorQue (intToNat 3) (intToNat 5)

    --Debe regresar True
    let prueba5 = menorQue (intToNat 3) (intToNat 5)

    --Debe regresar 34
    let prueba6 = natToInt $ intToNat 34

    --Debe regresar S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S 0)))))))))))))))))))))))))
    let prueba7 = intToNat 25

    --Debe regresar S(S(S(S(S(S(S(S(S(S 0))))))))))
    let prueba8 = longNat [1,2,3,4,5,6,7,8,9,10]

    --Debe regresar S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S 0))))))))))))))))))))))))
    let prueba9 =  factNat $ intToNat 4

    --Debe regresar S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S(S 0)))))))))))))))))))))
    let prueba10 = fibNat $ intToNat 8

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