-- Numeracion_de_ternas.hs
-- Numeración de las ternas de números naturales.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 2 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las ternas de números naturales se pueden ordenar como sigue
--    (0,0,0), 
--    (0,0,1),(0,1,0),(1,0,0),
--    (0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0),(2,0,0),
--    (0,0,3),(0,1,2),(0,2,1),(0,3,0),(1,0,2),(1,1,1),(1,2,0),(2,0,1),(2,1,0),(3,0,0),
--    ...
-- 
-- Definir la función
--    posicion :: (Int,Int,Int) -> Int
-- tal que (posicion (x,y,z)) es la posición de la terna de números
-- naturales (x,y,z) en la ordenación anterior. Por ejemplo,
--    posicion (0,1,0)  ==  2
--    posicion (0,0,2)  ==  4
--    posicion (0,1,1)  ==  5
-- ---------------------------------------------------------------------

import Test.HUnit

posicion1 :: (Int,Int,Int) -> Int
posicion1 (x,y,z) = length (takeWhile (/= (x,y,z)) ternas)

-- ternas es la lista ordenada de las ternas de números naturales. Por ejemplo,
--    ghci> take 10 ternas
--    [(0,0,0),(0,0,1),(0,1,0),(1,0,0),(0,0,2),(0,1,1),(0,2,0),(1,0,1),(1,1,0),(2,0,0)]
ternas :: [(Int,Int,Int)]
ternas = [(x,y,n-x-y) | n <- [0..], x <- [0..n], y <- [0..n-x]] 
