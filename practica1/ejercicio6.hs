-- Función para saber si un número es primo:
primo :: Integer -> Bool
primo n
  | n < 2     = False
  | n == 2    = True
  | otherwise = null [ x | x <- [2..n - 1], n `mod` x == 0]