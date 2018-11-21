--Estructuras Discretas 2019-1
--Profesor: Manuel Alcántara Juárez
--Ayudante: Rafael de Jesús García García
--Práctica 7
--Oscar Andres Rosas Hernandez


-- !!!!!!!!!!!!!!!!!! EL CASO DE PRUEBA 3 ESTA MAL !!!!!!!!!!!
module Main where

  -- Se define el tipo de dato para árboles binario como:
  -- 1. El árbol vacío
  -- 2. Node (elem, T1, T2), donde elem es el valor de un nodo del árbol y
  -- T1 y T2 son el subárbol izquierdo y derecho respectivamente
  -- Los elementos del árbol pueden ser de cualquier tipo, pero todos son del mismo tipo
  data AB a = Void | Nodo a (AB a) (AB a) deriving (Show, Eq)

  -- Regresa el número de nodos de un árbol
  numNodos :: AB a -> Int
  numNodos Void = 0 
  numNodos (Nodo root left right) = 1 + numNodos(left) + numNodos(right) 

  -- Regresa el número de hojas de un árbol
  -- Una hoja es un nodo que no tiene subárbol izquierdo ni derecho
  numHojas :: AB a -> Int
  numHojas Void = 0 
  numHojas (Nodo root Void Void) = 1 
  numHojas (Nodo root left right) = numHojas(left) + numHojas(right) 

  -- Regresa el número de nodos internos de un árbol.
  -- Un nodo interno es aquel que tiene al menos un subárbol, ya sea izquierdo o derecho
  nodosInt :: AB a -> Int
  nodosInt Void = 0 
  nodosInt (Nodo root Void Void) = 0
  nodosInt (Nodo root left right) = 1 + nodosInt(left) + nodosInt(right) 

  -- Regresa el número de aristas de un árbol.
  -- Una arista de un nodo es el número de subárboles que tiene (0, 1 ó 2).
  numAristas :: (Eq a) => AB a -> Int
  numAristas Void = 0
  numAristas (Nodo root left right) = 2 + numAristas(left) + numAristas(right) 

  -- Regresa los nodos internos de un árbol en una lista
  listaNodosInt :: AB a -> [a]
  listaNodosInt Void = []
  listaNodosInt (Nodo root Void Void) = []
  listaNodosInt (Nodo root left right) = [root] ++ listaNodosInt(left) ++ listaNodosInt(right) 

  -- Regresa las hojas de un árbol en una lista
  listaHojas :: AB a -> [a]
  listaHojas Void = []
  listaHojas (Nodo root Void Void) = [root]
  listaHojas (Nodo root left right) = listaHojas(left) ++ listaHojas(right) 

  -- Nos dice si el elemento que estamos buscando se encuentra en el árbol dado
  contiene :: (Eq a) => a -> AB a -> Bool
  contiene x Void = False
  contiene x (Nodo root left right) = do 
    if (root == x) then True else contiene x left || contiene x right 


  -- Hace un recorrido inorder a un árbol y regresa el recorrido en una lista
  inorder :: AB a -> [a]
  inorder Void = []
  inorder (Nodo root left right) = inorder(left) ++ [root] ++ inorder(right) 

  -- Hace un recorrido preorder a un árbol y regresa el recorrido en una lista
  preorder :: AB a -> [a]
  preorder Void = []
  preorder (Nodo root left right) = [root] ++ preorder(left) ++ preorder(right) 

  -- Hace un recorrido posorder a un árbol y regresa el recorrido en una lista
  postorder :: AB a -> [a]
  postorder Void = []
  postorder (Nodo root left right) = postorder(left) ++ postorder(right) ++ [root] 


  main = do
    let test = (Nodo 1
                        (Nodo 2
                          (Nodo 9
                            (Void)
                            (Nodo 10 (Void) (Void))
                          )
                          (Nodo 4
                            (Nodo 5 (Void) (Void))
                            (Nodo 6 (Void) (Void))
                          )
                        )
                        (Nodo 3
                          (Nodo 7 (Void) (Void))
                          (Nodo 8 (Void) (Void))
                        )
                      )

    -- Árbol de prueba 2
    let test2 = (Nodo 1
                        (Nodo 2
                          (Void)
                          (Nodo 4
                            (Nodo 5 (Void) (Void))
                            (Nodo 6 (Void) (Void))
                          )
                        )
                        (Nodo 3
                          (Nodo 7 (Void) (Void))
                          (Nodo 8 (Void) (Void))
                        )
                      )
    -- Árbol de prueba 3
    let test3 = (Nodo 1
                      (Nodo 2
                        (Void)
                        (Nodo 4
                          (Nodo 5 (Void) (Void))
                          (Nodo 6 (Void) (Void))
                        )
                      )
                      (Nodo 3
                        (Void)
                        (Void)
                      )
                    )

    --Debe regresar 10
    let prueba1 = numNodos test

    --Debe regresar 4
    let prueba2 = numHojas test2

    --Debe regresar 5
    let prueba3 = nodosInt test

    --Debe regresar [1,2,9,4,3]
    let prueba5 = listaNodosInt test

    --Debe regresar [5,6,3]
    let prueba6 = listaHojas test3

    --Debe regresar True
    let prueba7 = contiene 5 test

    --Debe regresar [9,10,2,5,4,6,1,7,3,8]
    let prueba8 = inorder test

    --Debe regresar [1,2,9,10,4,5,6,3,7,8]
    let prueba9 = preorder test

    --Debe regresar [10,9,5,6,4,2,7,8,3,1]
    let prueba10 = postorder test

    print prueba1
    print prueba2
    print prueba3
    print prueba5
    print prueba6
    print prueba7
    print prueba8
    print prueba9
    print prueba10
