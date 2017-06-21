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

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis F.
posicionA1 :: (Int,Int,Int) -> Int
posicionA1 t = aux t ternas
    where ternas = concat[[(x,y,z) | x <- [0..n], 
                                     y <- [0..n], 
                                     z <- [0..n], 
                                     x+y+z == n] | n <- [0..]]
          aux x (y:ys) | x == y    = 0
                       | otherwise = 1 + aux x ys

-- David
posicionA2 :: (Int,Int,Int) -> Int
posicionA2 (x,y,z) = length (takeWhile (/=(x,y,z)) (generaTernas 0))

generaTernas :: (Enum t, Eq t, Num t) => t -> [(t, t, t)]
generaTernas d = ternas ++ generaTernas (d+1)
    where ternas = [(a,b,c) | a <- [0..d], b <- [0..d], c <- [0..d],
                              a+b+c == d]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

posicion :: (Int,Int,Int) -> Int
posicion = posicionA2

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~: 
          posicion (0,1,0)  ~?=  2,
          "2" ~: "ej2" ~: 
          posicion (0,0,2)  ~?=  4,
          "3" ~: "ej3" ~: 
          posicion (0,1,1)  ~?=  5]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
