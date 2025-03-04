import Control.Monad (forM)
import Data.Maybe (fromJust)

data Prop   = Var String
            | Prop :& Prop
            | Prop :| Prop
            | Prop :/ Prop
            | No Prop
            deriving (Eq, Show)

--Ejercicio 2
--Funcion que identifica cuales son las variables proposicionales de una proposicion.

vars :: Prop -> [String]
vars (Var p)  = [p]
vars (p :& q) = vars p ++ vars q
vars (p :| q) = vars p ++ vars q
vars (p :/ q) = vars p ++ vars q
vars (No p)   = vars p


--Funcion que captura todas las posibles interpretaciones de las variables proposicionales en una lista.

valores :: [String] -> [[(String, Bool)]]
valores []     = [[]]
valores (x:xs) = [ (x,v):i | v<-[True,False], i<-valores xs ] 


--Funcion que le pregunta al usuario una valuacion particular para una lista de variables proposicionales

valuacionManual :: [String] -> IO [(String, Bool)]
valuacionManual	vars = forM vars $ \var -> do
    putStrLn $ "Ingresa interpretacion para " ++ var ++ " (0 o 1)"
    input <- getValidInput
    return (var, input)

getValidInput :: IO Bool
getValidInput = do 
	input <- getLine
	case input of 
	 "0" -> return False
	 "1" -> return True
	 _-> do 
 	  putStrLn "Entrada invalida. Ingresar True o False."
 	  getValidInput

--Ejercicio 3
--Funcion que calcula la interpretacion de una formula proposicional.
interpretacion :: Prop -> [(String, Bool)] -> Bool
interpretacion (Var p) i = fromJust (lookup p i)
interpretacion (p :& q) i = interpretacion p i && interpretacion q i
interpretacion (p :| q) i = interpretacion p i || interpretacion q i
interpretacion (p :/ q) i = not (interpretacion p i) || interpretacion q i
interpretacion (No p) i = not (interpretacion p i)


--Ejercicio 4
--Funcion que determina si una formula es tautologia.
esTautologia :: Prop -> Bool
esTautologia p = all (interpretacion p) (valores (vars p))