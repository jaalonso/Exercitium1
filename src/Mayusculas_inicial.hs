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

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Tamara Royán
-- ============

mayusculaInicialA1 :: String -> String
mayusculaInicialA1 [] = []
mayusculaInicialA1 (x:xs) = toUpper x : [toLower x | x <- xs]  

-- Julián Galindo
-- ==============

mayusculaInicialA2 :: String -> String
mayusculaInicialA2 [] = []
mayusculaInicialA2 (x:xs) = (toUpper x):[toLower n | n <- xs]

-- Jasone Ramírez
-- ==============

mayusculaInicialA3 :: String -> String
mayusculaInicialA3 [] = []
mayusculaInicialA3 xs = [x']++xs'
    where x' = toUpper (head xs)
          xs'= [toLower x | x <- tail xs]

-- Jesús Camacho
-- =============

mayusculaInicialA4 :: String -> String
mayusculaInicialA4 [] = []
mayusculaInicialA4 (x:xs) = primeraMayuscula x ++ letrasMinusculas xs

letrasMinusculas []     = []
letrasMinusculas (x:xs) = if ord x > 96 then x:letrasMinusculas xs
                          else chr (ord (x) + 32):letrasMinusculas xs

primeraMayuscula x = if ord x > 96 then [chr ((ord x)- 32)] else [x]

-- Rafael
-- ======

mayusculaInicialA5 :: String -> String
mayusculaInicialA5 (x:xs) = [chr (ord x - 32)] ++ todoaminusculas xs
mayusculaInicialA5 [] = []

todoaminusculas :: String -> String
todoaminusculas (x:xs) 
    | isLower x = [x]++ todoaminusculas xs
    | otherwise = [chr (ord x + 32)] ++ todoaminusculas xs
todoaminusculas [] = []


-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica = quickCheck prop_mayusculaInicial

-- La propiedad es
prop_mayusculaInicial :: String -> Bool
prop_mayusculaInicial xs = 
    mayusculaInicial xs == mayusculaInicialA4 xs

-- La comprobación es
--    ghci> quickCheck prop_mayusculaInicial
--    +++ OK, passed 100 tests.
