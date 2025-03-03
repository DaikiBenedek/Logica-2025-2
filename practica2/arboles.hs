import Data.Maybe

data ArbolBin = Vacio
              | Nodo Int ArbolBin ArbolBin
              deriving (Show, Eq)

count :: ArbolBin -> Int
count Vacio = 0
count (Nodo a i d) = 1 + count i + count d

height :: ArbolBin -> Int
height Vacio = 0
height (Nodo a i d) = 1 + max (height i) (height d)

arbolBalanceado :: Int -> ArbolBin
arbolBalanceado = arbolAux 0 0 

arbolAux :: Int -> Int -> Int -> ArbolBin
arbolAux l m z | 2^l + m -1 > z     = Vacio
               | otherwise       = Nodo (2^l + m -1) 
                                    (arbolAux (l+1) (2*m) z) 
                                    (arbolAux (l+1) (2*m+1) z)
