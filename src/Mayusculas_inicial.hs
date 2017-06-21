-- Mayusculas_inicial.hs
-- Mayúscula inicial.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  3 de Noviembre de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    mayusculaInicial :: String -> String
-- tal que (mayusculaInicial xs) es la palabra xs con la letra inicial
-- en mayúscula y las restantes en minúsculas. Por ejemplo, 
--    mayusculaInicial "sEviLLa"  ==  "Sevilla"
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Char

-- 1ª definición (por comprensión):
mayusculaInicial :: String -> String
mayusculaInicial [] = []
mayusculaInicial (x:xs) = toUpper x : [toLower x | x <- xs]

-- 2ª definición (por recursión):
mayusculaInicial2 :: String -> String
mayusculaInicial2 [] = []
mayusculaInicial2 (x:xs) = toUpper x : aux xs
    where aux (x:xs) = toLower x : aux xs
          aux []     = []
