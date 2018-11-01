--Estructuras Discretas 2019-1
--Profesor: Manuel Alcántara Juárez
--Ayudante: Rafael de Jesús García García
--Práctica 7
--Oscar Andres Rosas Hernandez


-- !!!!!!!!!!!!!!!!!! EL CASO DE PRUEBA 3 ESTA MAL !!!!!!!!!!!
module Main where

  -- Tipo para las variables propisicionales que será de tipo entero.
  -- Es decir el nombre de las variables será un entero.
  type Var = Int

  -- Tipo para las proposiciones lógicas.
  -- T representa el valor verdadero y F falso.
  -- P es una variable que recibe un nombre.
  -- Neg es la negación (~) y recibe una proposición.
  -- And es la conjunción (&) y recibe dos proposiciones.
  -- Or es la disyunción (|) y recibe dos proposiciones.
  -- Imp es la implicación (=>) y recibe dos proposiciones.
  -- Iff es el si y solo si (<=>) y recibe dos proposiciones.
  data Prop = T | F | P Var | Neg Prop | And Prop Prop | Or Prop Prop | Imp Prop Prop | Iff Prop Prop

  -- Conjunto donde guardaremos el valor de verdad de las variables.
  -- Si la variable se encuentra en el conjunto, su valor de verdad es T.
  -- Si no, es False.
  type Estado = [Var]

  -- Representación del tipo proposiciones
  -- valor: 1 punto
  instance Show Prop where
    show T = show "true"
    show F = show "false"
    show (P x) = "P" ++ show x
    show (Neg p) = "~ " ++ show p
    show (Or p1 p2) = "(" ++ show p1 ++ " | " ++ show p2 ++ ")"
    show (And p1 p2) = "(" ++ show p1 ++ " & " ++ show p2 ++ ")"
    show (Imp p1 p2) = "(" ++ show p1 ++ " -> " ++ show p2 ++ ")"
    show (Iff p1 p2) = "(" ++ show p1 ++ " <-> " ++ show p2 ++ ")"

  --Funcion que devuelve el conjunto de variables proposicionales
  -- valor: 1 punto

  hasUnico :: (Eq a) => [a] -> [a]
  hasUnico (x:xs) = x : hasUnico (filter (/= x) xs)
  hasUnico [] = []

  vars :: Prop -> [Var]
  vars T = []
  vars F = []
  vars (P x) = [x]
  vars (Neg p) = hasUnico (vars p)
  vars (Or p1 p2) = hasUnico  (vars p1 ++ vars p2) 
  vars (And p1 p2) = hasUnico (vars p1 ++ vars p2) 
  vars (Imp p1 p2) = hasUnico (vars p1 ++ vars p2) 
  vars (Iff p1 p2) = hasUnico (vars p1 ++ vars p2) 

  -- Dada una proposición y un estado, determinar el valor de verdad de la proposición
  -- valor: 2 puntos
  interp :: Prop -> Estado -> Bool
  interp T list = True
  interp F list = False
  interp (P x) (h:t) = if h == x then True else interp (P x) t
  interp (P x) [] = False
  interp (Neg p) x = not (interp p x)
  interp (Or p1 p2) x = (interp p1 x) || (interp p2 x) 
  interp (And p1 p2) x = (interp p1 x) && (interp p2 x) 
  interp (Imp p1 p2) x = not (interp p1 x) || (interp p2 x) 
  interp (Iff p1 p2) x = (interp p1 x) == (interp p2 x) 


  type Elem = Int
  data Lista = Vacia | Cons Elem Lista

  -- Representación en cadena de una Lista
  -- La representación es libre, sin embargo, debe abarcar todos los casos.
  -- Es decir, cualquier lista debe poder representarse.
  -- valor: 1 punto
  instance Show Lista where
    show Vacia = "[]"
    show (Cons h l) = "(" ++ (show h) ++ ":" ++ (show l) ++ ")" 

  -- Define la igualdad entre dos Listas
  -- valor: 1 punto
  instance Eq Lista where
    (==) Vacia Vacia = True
    (==) Vacia wea = False
    (==) (Cons h1 l1) (Cons h2 l2) = h1 == h2 && l1 == l2
      

  -- Recibe una Lista y devuelve su cabeza
  -- valor: 0.5 puntos
  cabeza :: Lista -> Int
  cabeza Vacia = error "Empty list"
  cabeza (Cons h l) = h

  -- Recibe una Lista y devuelve su cola
  -- valor: 0.5 puntos
  cola :: Lista -> Lista
  cola Vacia = Vacia
  cola (Cons h l) = l
   
  -- Suma todos los elementos de la Lista
  -- valor: 1 punto
  suma :: Lista -> Int
  suma Vacia = 0
  suma (Cons h l) = h + suma l

  -- Concatena dos listas
  -- valor: 1 punto
  concatenar :: Lista -> Lista -> Lista
  concatenar x Vacia = x
  concatenar Vacia x = x
  concatenar (Cons h1 l1) wea = Cons h1 (concatenar l1 wea)

  -- Obtiene la reversa de una Lista
  -- valor: 1 punto
  reversa :: Lista -> Lista
  reversa Vacia = Vacia
  reversa (Cons h1 l1) = concatenar (reversa l1) (Cons h1 Vacia)

  -- Revisa si un elemento pertenece a la Lista
  -- valor: 1 punto
  elementoDe :: Int -> Lista -> Bool
  elementoDe wea Vacia = False
  elementoDe wea (Cons h1 l1) = if wea == h1 then True else elementoDe wea l1


  main = do

    -- Pruebas --
    -- La representación en cadena de las listas es libre. En

    -- Debe imprimir T
    let prueba1 = T
    -- Debe imprimir (P 1 & P 2)
    let prueba2 = And (P 1) (P 2)
    -- Debe imprimir (~((P1 & P2) <=> (P2 | P3)) => F)
    let prueba3 = Imp (Neg $ Iff (And (P 1) (P 2)) (Or (P 2) (P 3))) F
    --Debe regresar [1,2]
    let prueba4 = vars prueba2
    --Debe regresar [1,2,3]
    let prueba5 = vars prueba3
    -- Debe regresar False
    let prueba6 = interp prueba2 [1]
    -- Debe regresar False
    let prueba7 = interp prueba3 [1,3]
    -- Debe regresar True
    let prueba8 = interp prueba3 [3]
    -- Debe regresar 3
    let prueba9 = cabeza $ Cons 3 $ Cons 5 $ Cons 10 Vacia
    -- Debe regresar (5:(10:[]))
    let prueba10 = cola $ Cons 3 $ Cons 5 $ Cons 10 Vacia
    -- Debe regresar 18
    let prueba11 = suma $ Cons 3 $ Cons 5 $ Cons 10 Vacia
    -- Debe regresar (3:(5:(10:(4:[]))))
    let prueba12 = concatenar (Cons 3 $ Cons 5 $ Cons 10 Vacia) (Cons 4 Vacia)
    -- Debe regresar (10:(5:(3:[])))
    let prueba13 = reversa $ Cons 3 $ Cons 5 $ Cons 10 Vacia
    -- Debe regresar False
    let prueba14 = elementoDe 6 $ Cons 3 $ Cons 5 $ Cons 10 Vacia
    -- Debe regresar True
    let prueba15 = elementoDe 5 $ Cons 3 $ Cons 5 $ Cons 10 Vacia

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