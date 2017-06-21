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

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Verónica Moise
-- ==============

divisoresConFinalA1 n x = [y | y <- divisores n, cifrasPI y x]

divisores n = [x | x <- [1..n], rem n x == 0]

cifras n | n < 10    = [n]
         | otherwise = (mod n 10) : cifras (n `div` 10)

-- cifrasPI n x | cifras x <= cifras n = take m (cifras n) == cifras x
--              | otherwise            = False
--     where m = length (cifras x)

cifrasPI n x = take m (cifras n) == cifras x
    where m = length (cifras x)

-- Luis
-- ====

divisoresConFinalA2 :: Integer -> Integer -> [Integer]
divisoresConFinalA2 n m = 
    [x | x <- divisores n, isSuffixOf (show m) (show x)]
    where divisores x = [y | y <- [1..x], x `mod` y == 0]

-- Laura
-- =====

divisoresConFinalA3 :: Integer -> Integer -> [Integer]
divisoresConFinalA3 n m = [x | x <- divisores n, digFinIg m x]
    where divisores n   = [x | x <- [1..n], rem n x == 0] 
          digFinIg  m x =  cifrasA3 m == drop p (cifrasA3 x) 
              where p = length (cifrasA3 x) - length (cifrasA3 m)

cifrasA3 :: Integer -> [Integer]
cifrasA3 m = [read [x] | x <- show m]

-- Victoria
-- ========

divisoresConFinalA4 :: Integer -> Integer -> [Integer]
divisoresConFinalA4 n m = 
    [x| x <- divisoresA4 n, 
        iniciales (reverse (digitosA4 x)) (reverse (digitosA4 m)) == True]
                      
digitosA4 :: Integer -> [Integer]
digitosA4 m = [read [x] | x <- show m]

divisoresA4 x = [n | n <- [1..x], mod x n == 0]

iniciales [] [] = True
iniciales xs [] = True
iniciales [] ys = False
iniciales(x:xs) (y:ys) | x==y      = iniciales xs ys      
                       | otherwise = False

-- Juan Flores
-- ===========

divisoresConFinalA5 n m = [x | x <- factores n, cifrados x m == m]

factores x = [n | n <- [1..x], mod x n == 0]

cifrados x m = listaNumero (reverse (take (length (cifrasA5 m)) (reverse (cifrasA5 x))))

listaNumero [a]    = a
listaNumero (x:xs) = x * 10^(length xs) + listaNumero xs

cifrasA5 x | x < 10    = [x]
           | otherwise = (cifrasA5 (div x 10)) ++ [rem x 10]

-- Ángela
-- ======

divisoresConFinalA6 :: Integer -> Integer -> [Integer]
divisoresConFinalA6 n m = 
    [x | x <- divisoresA6 n, 
         reverse (take (length ys) (reverse (show x))) == ys]
    where ys = show m

divisoresA6 n = [x | x <- [1..n], rem n x == 0]

-- Paco
-- ====

divisoresConFinalA7 n m =
    [x | x <- xs, take l (reverse (show x)) == reverse (show m)]
    where xs = divisoresA7 n
          l  = length (show m)

divisoresA7 n = [x | x <- [1..n `div` 2], n `rem` x == 0] ++ [n]

-- Daniel Sánchez
-- ==============

divisoresConFinalA8 n m = 
    [x | x <- divisores2 n, last (digitos3 x) == m]

digitos3 x | x < 10                = [x]
           | elem x (multiplos 10) = [x]
           | otherwise             = digitos3 (div x 10)++[rem x 10]

divisores2 x = [n | n <- [1..x], mod x n == 0]

multiplos x = [x*n | n <- [1..]]

-- Agustín Ruiz Poyato
-- ===================

divisoresConFinalA9 :: Integer -> Integer -> [Integer]
divisoresConFinalA9 n m = 
    [x | x <- divisoresA9 n, drop ((length (show x)-q)) (show x) == show m]
    where q = length (show m)

divisoresA9 n = [x | x <- [1..n], mod n x == 0]

-- Daniel Simon
-- ============

divisoresConFinalA10 n m =
    [t | t <- (divisoresA10 n), rem t (preciso m) == m]
    where preciso m | m < 10   = 10
                    | otherwise = 10 ^ (natural m)

natural m | m < 10    = 1
          | otherwise = 1 + natural (div m 10)

divisoresA10 n = [m | m <- [1..n], rem n m == 0]

-- Fran Valladares
-- ===============

divisoresConFinalA11 x n = cifrasLista (divisoresA11 x) n

cifrasLista xs n 
    | n == 1 = [x | x <- xs, take (length (cifrasA11 n)) (reverse (cifrasA11 x)) == cifrasA11 n]
    | n /= 1 = [x | x <- xs, drop (length (cifrasA11 x)- length (cifrasA11 n)) (cifrasA11 x) == cifrasA11 n]

divisoresA11 n = [m | m <- [1..n], rem n m == 0]

cifrasA11 :: Integer -> [Integer]
cifrasA11 x = [read [z] | z <- show x]

-- Ana Vidal
-- =========

divisoresConFinalA12 :: Integer -> Integer -> [Integer]
divisoresConFinalA12 n m = 
    [x | x <- divisoresA12 n, take z (reverse(digitosA12 x)) == reverse (digitosA12 m)]
    where z = length (digitosA12 m)

divisoresA12 n = [x | x <- [1..n], rem n x==0]

digitosA12 :: Integer -> [Integer]
digitosA12 x = [read [n] | n <- show x]

-- Rocío
-- =====

divisoresConFinalA13 n m | m < 10    = ys
                         | otherwise = zs
    where ys = [x | x <- factoresA13 n, last (digitosA13 x) == m]
          zs = [x | x <- factoresA13 n, rem x (10^a) == m]
          a  = length (digitosA13 m)

digitosA13 n = reverse (digitos' n)

digitos' n | n < 10    = [n]
           | otherwise = rem n 10 : digitos' (div n 10)

factoresA13 n = [x | x <- [1..n], rem n x == 0]

-- Luis Portillo
-- =============

divisoresConFinalA14 :: Integer -> Integer -> [Integer]
divisoresConFinalA14 n d = 
    [y | y <- divisoresA14 n, subcj (digitosA14 y) a == a ]
    where a = digitosA14 d

divisoresA14 x = [y | y <- [1..x], rem x y==0]

subcj [] _ = []
subcj xs ys | xs == ys  = xs
            | otherwise = subcj (tail xs) ys

digitosA14 :: Integer -> [Integer]
digitosA14 n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    divisoresConFinal 84 4    ==  [4,14,84]    &&
    divisoresConFinal 720 20  ==  [20,120,720]
    where divisoresConFinal = f

prop_equivalencia :: Integer -> Integer -> Bool
prop_equivalencia x y =
    divisoresConFinal1 x y' == divisoresConFinalA14 x y'
    where y' = 1000 + (abs y) `mod` 100
