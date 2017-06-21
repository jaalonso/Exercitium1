-- Filtro_booleano.hs
-- Filtro booleano.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  1 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    filtroBooleano :: [Bool] -> [a] -> [Maybe a]
-- tal que (filtroBooleano xs ys) es la lista de los elementos de ys
-- tales que el elemento de xs en la misma posición es verdadero. Por
-- ejemplo, 
--    ghci> filtroBooleano [True,False,True] "Sevilla"
--    [Just 'S',Nothing,Just 'v']
--    ghci> filtroBooleano (repeat True) "abc"
--    [Just 'a',Just 'b',Just 'c']
--    ghci> take 3 (filtroBooleano (repeat True) [1..])
--    [Just 1,Just 2,Just 3]
--    ghci> take 3 (filtroBooleano (repeat False) [1..])
--    [Nothing,Nothing,Nothing]
--    ghci> take 3 (filtroBooleano (cycle [True,False]) [1..])
--    [Just 1,Nothing,Just 3]
-- ---------------------------------------------------------------------

import Test.HUnit

-- 1ª solución (por comprensión):
filtroBooleano1 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano1 xs ys = [f (x,y) | (x,y) <- zip xs ys]
    where f (x,y) | x         = Just y
                  | otherwise = Nothing

-- 2ª solución (con zipWith):
filtroBooleano2 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano2 = zipWith f
    where f x y  | x         = Just y
                 | otherwise = Nothing

-- 3ª solución (con zipWith y lambda):
filtroBooleano3 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano3 = zipWith (\x y -> if x then Just y else Nothing)

-- 4ª solución (por recursión):
filtroBooleano4 :: [Bool] -> [a] -> [Maybe a]
filtroBooleano4 (x:xs) (y:ys) | x         = Just y : filtroBooleano4 xs ys
                              | otherwise = Nothing : filtroBooleano4 xs ys
filtroBooleano4 _ _                       = []
