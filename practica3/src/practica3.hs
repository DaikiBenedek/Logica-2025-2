import Control.Monad (forM)
import Data.Maybe (fromJust)
import Data.List (group)

-- Definición del tipo de datos para fórmulas proposicionales
data Prop = Var String
          | Prop :& Prop
          | Prop :| Prop
          | Prop :/ Prop
          | No Prop
          | Parentesis Prop -- Constructor para paréntesis
          deriving (Eq, Show)

-- Ejercicio 1
-- Función para obtener los factores primos de un número
factoresPrimos :: Int -> [Int]
factoresPrimos n = factoresPrimosAux n 2
  where
    factoresPrimosAux 1 _ = []
    factoresPrimosAux n divisor
      | n `mod` divisor == 0 = divisor : factoresPrimosAux (n `div` divisor) divisor
      | otherwise = factoresPrimosAux n (divisor + 1)

-- Función para obtener la factorización en potencias de primos
factorizacionPotencias :: Int -> [(Int, Int)]
factorizacionPotencias n = map (\xs -> (head xs, length xs)) (group (factoresPrimos n))
  where
    group [] = []
    group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

-- Ejercicio 2
-- Función que identifica las variables proposicionales de una proposición.
vars :: Prop -> [String]
vars (Var p)  = [p]
vars (p :& q) = vars p ++ vars q
vars (p :| q) = vars p ++ vars q
vars (p :/ q) = vars p ++ vars q
vars (No p)   = vars p

-- Función para asignar un número único a una fórmula proposicional
asignarNumero :: Prop -> Int
asignarNumero (Var "p") = 2
asignarNumero (Var "q") = 3
asignarNumero (Var "r") = 5
asignarNumero (No p) = 7 * asignarNumero p 
asignarNumero (p :& q) = 11 * asignarNumero p * asignarNumero q 
asignarNumero (p :| q) = 13 * asignarNumero p * asignarNumero q 
asignarNumero (p :/ q) = 17 * asignarNumero p * asignarNumero q 
asignarNumero (Parentesis p) = asignarNumero p 
asignarNumero _ = error "Solo se permiten las variables p, q y r."

-- Funciones auxiliares de la práctica 3
-- Función que captura todas las posibles interpretaciones de las variables proposicionales en una lista.
valores :: [String] -> [[(String, Bool)]]
valores []     = [[]]
valores (x:xs) = [ (x,v):i | v <- [True, False], i <- valores xs ]

-- Función que le pregunta al usuario una valuación particular para una lista de variables proposicionales
valuacionManual :: [String] -> IO [(String, Bool)]
valuacionManual vars = forM vars $ \var -> do
    putStrLn $ "Ingresa interpretación para " ++ var ++ " (0 o 1)"
    input <- getValidInput
    return (var, input)

getValidInput :: IO Bool
getValidInput = do 
    input <- getLine
    case input of 
        "0" -> return False
        "1" -> return True
        _   -> do 
            putStrLn "Entrada inválida. Ingresar 0 o 1."
            getValidInput

-- Función que calcula la interpretación de una fórmula proposicional.
interpretacion :: Prop -> [(String, Bool)] -> Bool
interpretacion (Var p) i = fromJust (lookup p i)
interpretacion (p :& q) i = interpretacion p i && interpretacion q i
interpretacion (p :| q) i = interpretacion p i || interpretacion q i
interpretacion (p :/ q) i = not (interpretacion p i) || interpretacion q i
interpretacion (No p) i = not (interpretacion p i)

-- Función que determina si una fórmula es tautología.
esTautologia :: Prop -> Bool
esTautologia p = all (interpretacion p) (valores (vars p))

-- Ejercicio 3
-- Función para verificar si un número está asociado a una fórmula válida y si es una tautología
esFormulaValida :: Int -> Bool
esFormulaValida n = any (\f -> asignarNumero f == n && esTautologia f) formulas
  where
    -- Generar algunas fórmulas válidas
    formulas = [ Var "p"
               , Var "q"
               , Var "r"
               , No (Var "p")
               , Var "p" :& Var "q"
               , Var "p" :| Var "q"
               , Var "p" :/ Var "q"
               , Parentesis (Var "p" :& Var "q")
               , No (Var "p") :| Var "q"
               , Var "p" :/ (Parentesis (Var "q" :& Var "r"))
               ]
