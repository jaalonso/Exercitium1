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
