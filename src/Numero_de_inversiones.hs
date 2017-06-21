-- Numero_de_inversiones.hs
-- Número de inversiones.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se dice que en una sucesión de números x(1), x(2), ..., x(n) hay una 
-- inversión cuando existe un par de números x(i) > x(j), siendo i < j.
-- Por ejemplo, en la permutación 2, 1, 4, 3 hay dos inversiones 
-- (2 antes que 1 y 4 antes que 3) y en la permutación 4, 3, 1, 2 hay 
-- cinco inversiones (4 antes 3, 4 antes 1, 4 antes 2, 3 antes 1, 
-- 3 antes 2).
-- 
-- Definir la función 
--    numeroInversiones :: Ord a => [a] -> Int  
-- tal que (numeroInversiones xs) es el número de inversiones de xs. Por
-- ejemplo, 
--    numeroInversiones [2,1,4,3]  ==  2
--    numeroInversiones [4,3,1,2]  ==  5
-- ---------------------------------------------------------------------

import Test.HUnit
import Data.Array

-- 1ª solución (por recursión)
numeroInversiones1 :: Ord a => [a] -> Int  
numeroInversiones1 [] = 0
numeroInversiones1 (x:xs) =
    length (filter (x>) xs) + numeroInversiones1 xs

-- 2ª solución (por comprensión)
numeroInversiones2 :: Ord a => [a] -> Int  
numeroInversiones2 xs =
    length [(i,j) | i <- [0..n-2], j <- [i+1..n-1], xs!!i > xs!!j]
    where n = length xs

-- 3ª solución (con vectores)
numeroInversiones3 :: Ord a => [a] -> Int  
numeroInversiones3 xs =
    length [(i,j) | i <- [1..n-1], j <- [i+1..n], v!i > v!j]
    where n = length xs
          v = listArray (1,n) xs

-- Solución de Mª José
numeroInversionesMJ :: Ord a => [a] -> Int  
numeroInversionesMJ xs = 
    sum [numeroInversionesI xs i| i <-[1..(length xs)]]
    where numeroInversionesI xs i = 
              length [(j,x) | (j,x) <- zip [1..] xs,
                              i < j, x < xs!!(i-1)]
