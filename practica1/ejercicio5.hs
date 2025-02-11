-- devuelve el nùmero anterior de un numero natural n usando patrones
anteriorPatrones :: Integer -> Integer
anteriorPatrones 1 = 0
anteriorPatrones n = n - 1 

-- lo mismo que la funciòn anterior pero con guardas
anteriorGuardas :: Integer -> Integer
anteriorGuardas n
        | n == 1         = 0
        | otherwise      = n - 1

-- verificación de equivalencia
equivalenciaAnterior :: Integer -> Bool
equivalenciaAnterior n = anteriorGuardas n == anteriorPatrones n
