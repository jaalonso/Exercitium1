-- Numeros_con_digitos_primos.hs
-- Números con todos sus dígitos primos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La sucesión A046034 de la OEIS (The On-Line Encyclopedia of Integer
-- Sequences) está formada por los números tales que todos sus dígitos
-- son primos. Los primeros términos de A046034 son 
--    2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223
-- 
-- Definir la constante
--    numerosDigitosPrimos :: [Int]
-- cuyos elementos son los términos de la sucesión A046034. Por ejemplo,
--    ghci> take 22 numerosDigitosPrimos
--    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]
-- ¿Cuántos elementos hay en la sucesión menores que 2013?
-- ---------------------------------------------------------------------


import Data.Numbers.Primes -- Para A1
import Data.Char           -- Para A1
import Data.List (unfoldr) -- Para A5

numerosDigitosPrimos :: [Int]
numerosDigitosPrimos = 
    [n | n <- [2..], digitosPrimos n]

-- (digitosPrimos n) se verifica si todos los dígitos de n son
-- primos. Por ejemplo,
--    digitosPrimos 352  ==  True
--    digitosPrimos 362  ==  False
digitosPrimos :: Int -> Bool
digitosPrimos n = all (`elem` "2357") (show n)

-- 2ª definición de digitosPrimos:
digitosPrimos2 :: Int -> Bool
digitosPrimos2 n = subconjunto (cifras n) [2,3,5,7]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
cifras :: Int -> [Int]
cifras n = [read [x] | x <-show n]

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo, 
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- El cálculo es
--    ghci> length (takeWhile (<2013) numerosDigitosPrimos)
--    84
