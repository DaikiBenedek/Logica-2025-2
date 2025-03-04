import Data.Maybe

data ArbolBin = Vacio
              | Nodo Int ArbolBin ArbolBin
              deriving (Show, Eq)

--Ejercicio1
--Funcion que cuenta los nodos en un arbol binario
count :: ArbolBin -> Int
count Vacio = 0
count (Nodo a i d) = 1 + count i + count d

--Funcion que devuelve la altura de un arbol binario
height :: ArbolBin -> Int
height Vacio = 0
height (Nodo a i d) = 1 + max (height i) (height d)

--Funcion que dado un entero no negativo k, devuelve un arbol binario balanceado que contenga en sus nodos a todos los enteros no negativos tal que z<=k
arbolBalanceado :: Int -> ArbolBin
arbolBalanceado = arbolAux 0 0 

arbolAux :: Int -> Int -> Int -> ArbolBin
arbolAux l m z | 2^l + m -1 > z     = Vacio
               | otherwise       = Nodo (2^l + m -1) 
                                    (arbolAux (l+1) (2*m) z) 
                                    (arbolAux (l+1) (2*m+1) z)
