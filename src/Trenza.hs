-- Trenza.hs
-- Trenzado de listas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 9 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función 
--    trenza :: [a] -> [a] -> [a]
-- tal que (trenza xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--    trenza [5,1] [2,7,4]             ==  [5,2,1,7]
--    trenza [5,1,7] [2..]             ==  [5,2,1,3,7,4]
--    trenza [2..] [5,1,7]             ==  [2,5,3,1,4,7]
--    take 8 (trenza [2,4..] [1,5..])  ==  [2,1,4,5,6,9,8,13]
-- ---------------------------------------------------------------------

import Test.HUnit

-- 1ª definición (por comprensión):
trenza1 :: [a] -> [a] -> [a]
trenza1 xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- 2ª definición (por zipWith):
trenza2 :: [a] -> [a] -> [a]
trenza2 xs ys = concat (zipWith par xs ys)
    where par x y = [x,y]

-- 3ª definición (por zipWith y sin argumentos):
trenza3 :: [a] -> [a] -> [a]
trenza3 = (concat .) . zipWith par
    where par x y = [x,y]

-- 4ª definición (por recursión):
trenza4 :: [a] -> [a] -> [a]
trenza4 (x:xs) (y:ys) = x : y : trenza xs ys
trenza4 _      _      = []
