-- Divisores_con_final.hs
-- Divisores de un número con final dado.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 10 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    divisoresConFinal :: Integer -> Integer -> [Integer]
-- tal que (divisoresConFinal n m) es la lista de los divisores de n
-- cuyos dígitos finales coincide con m. Por ejemplo,
--    divisoresConFinal 84 4    ==  [4,14,84]
--    divisoresConFinal 720 20  ==  [20,120,720]
-- ---------------------------------------------------------------------

import Data.List (isSuffixOf)
import Test.QuickCheck

-- 1ª solución
-- ===========

divisoresConFinal1 :: Integer -> Integer -> [Integer]
divisoresConFinal1 n m = 
    [x | x <- [1..n], n `rem` x == 0, final x m]

--    final 325 5   ==  True
--    final 325 25  ==  True
--    final 325 35  ==  False
final :: Integer -> Integer -> Bool
final x y = take n xs == ys
    where xs = reverse (show x)
          ys = reverse (show y)
          n  = length ys

-- 2ª solución
-- ===========

divisoresConFinal2 :: Integer -> Integer -> [Integer]
divisoresConFinal2 n m = 
    [x | x <- [1..n], n `rem` x == 0, show m `isSuffixOf` show x]


-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en el problema 474 del proyecto Euler que se encuentra 
-- en http://bit.ly/1kVL56I
