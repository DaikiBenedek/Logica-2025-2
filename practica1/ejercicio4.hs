-- Implementaciones del factorial vistas en clase
fact1 :: Integer -> Integer
fact1 n = if n == 0 then 1
            else n * fact1 (n-1)

fact2 :: Integer -> Integer
fact2 n 
        | n == 0    = 1
        | otherwise = n * fact2 (n-1)

fact3 :: Integer -> Integer
fact3 0 = 1
fact3 n = n * fact3 (n-1)

fact4 :: Integer -> Integer
fact4 n
        | n == 0    = 1
        | n >= 1    = n * fact4 (n-1)

fact5 :: Integer -> Integer
fact5 0       = 1
fact5 (n + 1) = (n + 1) * fact5 n

fact6 :: Integer -> Integer
fact6 n = product [1 .. n]

fact7 :: Integer -> Integer
fact7 n = foldr (*) 1 [1 .. n]

-- función que comprueba la equivalencia de las implementaciones anteriores
factEquivalencia :: Integer -> Bool
factEquivalencia n = (fact1 n == fact2 n) &&
                     (fact2 n == fact3 n) &&
                     (fact3 n == fact4 n) &&
                     (fact4 n == fact5 n) &&
                     (fact5 n == fact6 n) &&
                     (fact6 n == fact7 n)
