-- Mayor_sucesion_3n+1.hs
-- Mayor sucesión del problema 3n+1.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 14 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La sucesión 3n+1 generada por un número entero positivo x es la
-- sucesión generada por el siguiente algoritmo: Se empieza con el
-- número x. Si x es par, se divide entre 2. Si x es impar, se
-- multiplica por 3 y se le suma 1. El  proceso se repite con el número
-- obtenido hasta que se alcanza el valor 1. Por ejemplo, la sucesión de
-- números generadas cuando se empieza en 22 es
--    22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1
-- Se ha conjeturado (aunque no demostrado) que este algoritmo siempre
-- alcanza el 1 empezando en cualquier entero positivo.
-- 
-- Definir la función
--    mayorLongitud :: Integer -> Integer -> Integer
-- tal que (mayorLongitud i j) es el máximo de las longitudes de las
-- sucesiones 3n+1 para todos los números comprendidos entre i y j,
-- ambos inclusives. Por ejemplo,
--    mayorLongitud   1   10  ==  20
--    mayorLongitud 100  200  ==  125
--    mayorLongitud 201  210  ==  89
--    mayorLongitud 900 1000  ==  174
-- ---------------------------------------------------------------------

import Data.List   -- para la solución A1

-- 1ª solución
-- ===========

mayorLongitud1 :: Int -> Int -> Int
mayorLongitud1 i j = maximum [length (sucesion k) | k <- [i..j]]

-- (sucesion n) es la sucesión 3n+1 generada por n. Por ejemplo, 
--    sucesion 22  ==  [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
sucesion :: Int -> [Int]
sucesion 1 = [1]
sucesion n | even n    = n : sucesion (n `div` 2)
           | otherwise = n : sucesion (3*n+1)

-- 2ª solución
-- ===========

mayorLongitud2 :: Int -> Int -> Int
mayorLongitud2 i j = maximum [longitud k | k <- [i..j]]

-- (longitud n) es la longitud de la sucesión 3n+1 generada por n. Por
-- ejemplo, 
--    longitud 22  ==  16
longitud :: Int -> Int
longitud 1 = 1
longitud n | even n    = 1 + longitud (n `div` 2)
           | otherwise = 1 + longitud (3*n+1)

-- 3ª solución (con iterate)
-- =========================

mayorLongitud3 :: Int -> Int -> Int
mayorLongitud3 i j = maximum [length (sucesion2 k) | k <- [i..j]]

-- (sucesion2 n) es la sucesión 3n+1 generada por n. Por ejemplo, 
--    sucesion2 22  ==  [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
sucesion2 :: Int -> [Int]
sucesion2 n = takeWhile (/=1) (iterate f n) ++ [1]
    where f x | even x    = x `div` 2
              | otherwise = 3*x+1

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Luis
-- ====
-- import Data.List

mayorLongitudA1 :: Integer -> Integer -> Integer
mayorLongitudA1 i j = maximum [genericLength (suc3 x) | x <- [i,i+1..j]]
    where suc3 x | even x    = takeWhile (>1) (x:suc3 (x`div`2))++[1]
                 | otherwise = takeWhile (>1) (x:suc3 (3*x+1))++[1]


-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    mayorLongitud   1   10  ==  20  &&
    mayorLongitud 100  200  ==  125 &&
    mayorLongitud 201  210  ==  89  &&
    mayorLongitud 900 1000  ==  174
    where mayorLongitud = f

-- Verificación
--    ghci> verifica mayorLongitud1
--    True
--    ghci> verifica mayorLongitud2
--    True

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en el ejercicio [The 3n + 1 problem](http://bit.ly/SIcT1Q) de 
-- [UVa Online Judge](http://uva.onlinejudge.org).
