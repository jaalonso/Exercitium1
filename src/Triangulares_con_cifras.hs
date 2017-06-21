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
