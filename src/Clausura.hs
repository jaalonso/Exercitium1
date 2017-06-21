-- Clausura.hs
-- Clausura de un conjunto respecto de una función
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un conjunto A está cerrado respecto de una función  f si para todo
-- elemento x de A se tiene que f(x) pertenece a A. La clausura de un
-- conjunto B respecto de una función f es el menor conjunto A que
-- contiene a B y es cerrado respecto de f. Por ejemplo, la clausura de
-- {0,1,2] respecto del opuesto es {0,1,2,-1,-2}. 
-- 
-- Definir la función  
--    clausura :: Eq a => (a -> a) -> [a] -> [a]
-- tal que (clausura f xs) es la clausura de xs respecto de f. Por
-- ejemplo, 
--    clausura (\x -> -x) [0,1,2]         ==  [0,1,2,-1,-2]
--    clausura (\x -> (x+1) `mod` 5) [0]  ==  [0,1,2,3,4]
-- ---------------------------------------------------------------------

import Data.List (nub,(\\))

clausura :: Eq a => (a -> a) -> [a] -> [a]
clausura f xs = clausura' f xs xs
    where clausura' f xs ys | null zs = ys
                            | otherwise = clausura' f zs (ys++zs)
              where zs = nuevosSucesores f xs ys

nuevosSucesores :: Eq a => (a -> a) -> [a] -> [a] -> [a]
nuevosSucesores f xs ys = nub [f x | x <- xs] \\ ys

