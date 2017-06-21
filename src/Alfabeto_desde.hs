-- Alfabeto_desde.hs
-- Alfabeto comenzando en un carácter.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 28 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    alfabetoDesde :: Char -> String
-- tal que (alfabetoDesde c) es el alfabeto, en minúscula, comenzando en
-- el carácter c, si c es una letra minúscula y comenzando en 'a', en
-- caso contrario. Por ejemplo,
--    alfabetoDesde 'e'  ==  "efghijklmnopqrstuvwxyzabcd"
--    alfabetoDesde 'a'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde '7'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde '{'  ==  "abcdefghijklmnopqrstuvwxyz"
--    alfabetoDesde 'B'  ==  "abcdefghijklmnopqrstuvwxyz"
-- ---------------------------------------------------------------------

import Data.Char (ord)
import Data.Tuple (swap)
import Test.HUnit

-- 1ª definición (con dropWhile y takeWhile):
alfabetoDesde1 :: Char -> String
alfabetoDesde1 c =
    dropWhile (<c) ['a'..'z'] ++ takeWhile (<c) ['a'..'z']

-- 2ª definición (con span):
alfabetoDesde2 :: Char -> String
alfabetoDesde2 c = ys ++ xs
    where (xs,ys) = span (<c) ['a'..'z']

-- 3ª definición (con break):
alfabetoDesde3 :: Char -> String
alfabetoDesde3 c = ys ++ xs
    where (xs,ys) = break (==c) ['a'..'z']

-- 4ª definición (sin argumentos):
alfabetoDesde4 :: Char -> String
alfabetoDesde4 = uncurry (++) . swap . flip span ['a'..'z'] . (>)

-- Ejemplo de cálculo:
--    alfabetoDesde4 'e'
--    = (uncurry (++) . swap . flip span ['a'..'z'] . (>)) 'e'
--    = (uncurry (++) . swap) ("abcd","efghijklmnopqrstuvwxyz")
--    = uncurry (++) ("efghijklmnopqrstuvwxyz","abcd")
--    = (++) "efghijklmnopqrstuvwxyz" "abcd"
--    = "efghijklmnopqrstuvwxyzabcd"

-- 5ª definición (sin argumentos):
alfabetoDesde5 :: Char -> String
alfabetoDesde5 = uncurry (flip (++)) . (`break` ['a'..'z']) . (==)

-- Ejemplo de cálculo:
--    alfabetoDesde5 'e'
--    = (uncurry (flip (++)) . (`break` ['a'..'z']) . (==)) 'e'
--    = uncurry (flip (++)) ("abcd","efghijklmnopqrstuvwxyz")
--    = flip (++) "abcd" "efghijklmnopqrstuvwxyz"
--    = "efghijklmnopqrstuvwxyz" ++ "abcd"
--    = "efghijklmnopqrstuvwxyzabcd"

-- 6ª definición (por comprensión):
alfabetoDesde6 :: Char -> String
alfabetoDesde6 c
    | c >= 'a' && c <= 'z' = [c..'z'] ++ ['a'..pred c]
    | otherwise            = ['a'..'z']
