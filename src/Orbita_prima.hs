-- Orbita_prima.hs
-- Órbita prima.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  6 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La órbita prima de un número n es la sucesión construida de la
-- siguiente forma: 
--    * si n es compuesto su órbita no tiene elementos 
--    * si n es primo, entonces n está en su órbita; además, sumamos n y
--      sus dígitos, si el resultado es un número primo repetimos el
--      proceso hasta obtener un número compuesto. 
-- Por ejemplo, con el 11 podemos repetir el proceso dos veces
--    13 = 11+1+1
--    17 = 13+1+3
-- Así, la órbita prima de 11 es 11, 13, 17. 
-- 
-- Definir la función
--    orbita :: Integer -> [Integer]
-- tal que (orbita n) es la órbita prima de n. Por ejemplo,
--    orbita 11 == [11,13,17]
--    orbita 59 == [59,73,83]
-- Calcular el menor número cuya órbita prima tiene más de 3 elementos.
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
-- =============================

orbita1 :: Integer -> [Integer]
orbita1 n | not (esPrimo n) = []
          | otherwise       = n : orbita1 (n + sum (cifras n))

esPrimo :: Integer -> Bool
esPrimo n = [x | x <- [1..n], n `rem` x == 0] == [1,n] 

cifras :: Integer -> [Integer]
cifras n = [read [x]| x <- show n]

-- El cálculo es
--    ghci> head [x | x <- [1,3..], length (orbita x) > 3]
--    277
-- 
--    ghci> orbita 277
--    [277,293,307,317]

-- 2ª definición (con iterate)
-- ===========================

orbita2 :: Integer -> [Integer]
orbita2 n = takeWhile esPrimo (iterate f n)
    where f x = x + sum (cifras x)

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis F.
-- =======
orbitaA1 :: Integer -> [Integer]
orbitaA1 n | primo n   = n : orbitaA1 (n + sum (digitos n))
           | otherwise = []
           where primo n     = divisores n == [1,n]
                 divisores n = [x | x <- [1..n], n `mod` x == 0]
                 digitos n   = [read [x] | x <- show n]

menorOrb3 :: Integer
menorOrb3 = head [n | n <- primos, length (orbitaA1 n) > 3]
    where primos      = [x | x <- [1..], divisores x == [1,x]]
          divisores n = [x | x <- [1..n], n `mod` x == 0]

-- Alberto Caro
-- ============

orbitaA2 x | primo x   = [x] ++ orbitaA2 (sum (digitos x) + x)
           | otherwise = []

divisores x = [y | y <- [1..x], mod x y == 0]

primo x = divisores x == [1,x]

digitos x | x < 10    = [x]
          | otherwise = digitos (div x 10) ++ [mod x 10]

-- José Ramón Sánchez
-- ==================

orbitaA3 :: Int -> [Int]
orbitaA3 n | primo n == False = []
           | otherwise        = n : orbitaA3 (sum (cifrasA3 n) + n)

cifrasA3 k | k < 10    = [k]
           | otherwise = cifrasA3 (div k 10) ++ [rem k 10]

-- Laura
-- =====

orbitaA4 :: Integer -> [Integer]
orbitaA4 n = takeWhile (noEsComp) (orbitaA4s n)
    where noEsComp x  = length (divisores x) == 2
          divisores x = [d | d <- [1..x], rem x d == 0]
          orbitaA4s x = x : [y + sum (cifrasA4 y) | y <- orbitaA4s x]

cifrasA4 :: Integer -> [Integer]
cifrasA4 m = [read [x] | x <- show m]

menor = head (head [orbitaA4 n | n <- [1..], length (orbitaA4 n) > 3])

-- Verónica Moise
-- ==============

orbitaA5 n | esPrimoA5 n = n : orbitaA5 (n + sumaCifras n)
           | otherwise = []

esPrimoA5 n = factores n == [1,n]

factores n = [x| x<-[1..n], mod n x == 0]

sumaCifras n | n < 10    = n
             | otherwise = mod n 10 + sumaCifras (div n 10)

-- Ángela
-- ======

orbitaA6 :: Integer -> [Integer]
orbitaA6 n | primoA6 n = n : orbitaA6 (n + sum (cifrasA6 n))
           | otherwise = []           

primoA6 n = [x | x <- [1..n], rem n x == 0]==[1,n]

cifrasA6 :: Integer -> [Integer]
cifrasA6 n = [read [x] | x <- show n]

--Menor órbita:
menorOrbita = head [n | n <- primos, length (orbitaA6 n) > 3]

primos :: Integral a => [a]
primos = criba [2..]

criba (p:xs) = p: criba [x | x <- xs, rem x p /= 0]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    orbita 11 == [11,13,17] &&
    orbita 59 == [59,73,83]
    where orbita = f

