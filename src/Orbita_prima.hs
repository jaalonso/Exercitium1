-- |
-- Module      : Orbita_prima
-- Description : Órbita prima.
-- Copyright   : Exercitium (13-06-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Órbita prima__
-- 
-- La órbita prima de un número n es la sucesión construida de la
-- siguiente forma:
--
-- * si n es compuesto su órbita no tiene elementos 
-- * si n es primo, entonces n está en su órbita; además, sumamos n y
--   sus dígitos, si el resultado es un número primo repetimos el
--   proceso hasta obtener un número compuesto. 
--
-- Por ejemplo, con el 11 podemos repetir el proceso dos veces
--
-- *  3 = 11+1+1
-- * 17 = 13+1+3
-- 
-- Así, la órbita prima de 11 es 11, 13, 17. 
-- 
-- Definir la función
-- 
-- > orbita :: Integer -> [Integer]
-- 
-- tal que __(orbita n)__ es la órbita prima de n. Por ejemplo,
-- 
-- >>> orbita 11
-- [11,13,17]
-- >>> orbita 59
-- [59,73,83]
-- 
-- Calcular el menor número cuya órbita prima tiene más de 3 elementos.

module Orbita_prima where

import Test.QuickCheck

-- | 1ª definición (por recursión)
orbita :: Integer -> [Integer]
orbita n | not (esPrimo n) = []
         | otherwise       = n : orbita (n + sum (cifras n))

-- | __(esPrimo n)__ se verifica si n es primo. Por ejemplo,
--
-- >>> esPrimo 17
-- True
-- >>> esPrimo 21
-- False
esPrimo :: Integer -> Bool
esPrimo n = [x | x <- [1..n], n `rem` x == 0] == [1,n] 

-- | __(cifras n)__ es la lista de las cifras de n. Por ejemplo,
--
-- >>> cifras 32542
-- [3,2,5,4,2]
cifras :: Integer -> [Integer]
cifras n = [read [x]| x <- show n]

-- | 2ª definición (con iterate)
orbita2 :: Integer -> [Integer]
orbita2 n = takeWhile esPrimo (iterate f n)
  where f x = x + sum (cifras x)

-- | __(prop_equiv_orbita n)__ se verifica si las definiciones de
-- 'orbita' son equivalentes sobre n. Por ejemplo,
-- 
-- >>> all prop_equiv_orbita [11, 59]
-- True
prop_equiv_orbita :: Integer -> Bool
prop_equiv_orbita n =
  orbita n == orbita2 n

-- | Comprueba la equivalencia de las definiciones de 'orbita'.
-- 
-- >>> verifica_equiv_orbita
-- +++ OK, passed 100 tests.
verifica_equiv_orbita :: IO ()
verifica_equiv_orbita =
  quickCheck prop_equiv_orbita

-- El cálculo es
--    > head [x | x <- [1,3..], length (orbita x) > 3]
--    277
-- 
--    > orbita 277
--    [277,293,307,317]
