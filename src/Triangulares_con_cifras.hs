-- Triangulares_con_cifras.hs
-- Números triangulares con n cifras distintas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 8 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los números triangulares se forman como sigue
-- 
--    *     *      * 
--         * *    * *
--               * * *
--    1     3      6
-- 
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
--     1 = 1
--     3 = 1+2
--     6 = 1+2+3
--    10 = 1+2+3+4
--    15 = 1+2+3+4+5
-- 
-- Definir la función
--    triangularesConCifras :: Int -> [Integer]
-- tal que (triangulares n) es la lista de los números triangulares con
-- n cifras distintas. Por  ejemplo, 
--    take 6 (triangularesConCifras 1)   ==  [1,3,6,55,66,666]
--    take 6 (triangularesConCifras 2)   ==  [10,15,21,28,36,45]
--    take 6 (triangularesConCifras 3)   ==  [105,120,136,153,190,210]
--    take 5 (triangularesConCifras 4)   ==  [1035,1275,1326,1378,1485]
--    take 2 (triangularesConCifras 10)  ==  [1062489753,1239845706]
-- ---------------------------------------------------------------------

import Data.List (nub)
import Test.HUnit

triangularesConCifras1 :: Int -> [Integer]
triangularesConCifras1 n =
    [x | x <- triangulares, nCifras x == n]

-- 1ª definición de triangulares (por comprensión)
-- ===============================================
triangulares1 :: [Integer]
triangulares1 = 1 : [x+y | (x,y) <- zip [2..] triangulares1]

-- 2ª definición de triangulares (usando scanl)
-- ============================================
triangulares2 :: [Integer]
triangulares2 = scanl (+) 1 [2..]

-- 3ª definición de triangulares (con la fórmula)
-- ==============================================
triangulares3 :: [Integer]
triangulares3 = [(n*(n+1)) `div` 2 | n <- [1..]]

-- Usaremos como triangulares la 2ª definición:
triangulares :: [Integer]
triangulares = triangulares2

-- (nCifras x) es el número de cifras distintas del número x. Por
-- ejemplo, 
--    nCifras 325275  ==  4
nCifras :: Integer -> Int
nCifras = length . nub . show

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
triangularesConCifrasA1 :: Int -> [Integer]
triangularesConCifrasA1 n = 
    [y | y <- [m*(m+1) `div` 2 | m <- [1..]], length (nub (digitos y)) == n]
    where digitos x | x <= 9    = [x]
                    | otherwise = x `mod` 10 : digitos (x `div` 10)

-- Laura
triangularesConCifrasA2 :: Int -> [Integer]
triangularesConCifrasA2 n = [triangular x | x <- [1..], cifrasDistTriang x == n]

cifrasDistTriang :: Integer -> Int
cifrasDistTriang m = length (nub (cifras (triangular m)))

cifras :: Integer -> [Integer]
cifras m = [read [x] | x <- show m]

triangular :: Integer -> Integer
triangular n = n*(n+1) `div` 2

-- Ángela
-- ======

triangularesConCifrasA3 :: Int -> [Integer]
triangularesConCifrasA3 n = 
    [y | y <- triangular, length (nub (cifras y)) == n]
    where triangular = [div (x*(x+1)) 2 | x <- [1..]]
          cifras :: Integer -> [Integer]
          cifras n = [read [x] | x <- show n]

-- David
-- =====

-- import Data.List

triangularesConCifrasA4 :: Int -> [Integer]
triangularesConCifrasA4 n = 
    [t | t <- triangularesA4, length (nub (show t)) == n]

triangularesA4 = 1:zipWith (+) [2..] triangularesA4

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

triangularesConCifras :: Int -> [Integer]
triangularesConCifras = triangularesConCifrasA4

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          take 6 (triangularesConCifras 1)   ~?=  [1,3,6,55,66,666],
          "2" ~: "ej2" ~:
          take 6 (triangularesConCifras 2)   ~?=  [10,15,21,28,36,45],
          "3" ~: "ej3" ~:
          take 6 (triangularesConCifras 3)   ~?=  [105,120,136,153,190,210],
          "4" ~: "ej4" ~:
          take 5 (triangularesConCifras 4)   ~?=  [1035,1275,1326,1378,1485],
          "5" ~: "ej5" ~:
          take 2 (triangularesConCifras 10)  ~?=  [1062489753,1239845706]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}

-- ---------------------------------------------------------------------
-- §  Referencias                                                     --
-- ---------------------------------------------------------------------

-- + M. Keith [On repdigit polygonal numbers](http://bit.ly/1hzZrDk).
-- + OEIS [A045914: Triangular numbers with all digits the
--   same](https://oeis.org/A045914) 
-- + OEIS [A213516: Triangular numbers having only 1 or 2 different
--   digits in base 10](https://oeis.org/A213516). 
-- + [Series of natural numbers which has all same
--   digits](http://bit.ly/1hA0Sl4).
