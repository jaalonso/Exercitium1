-- |
-- Module      : Divisores_con_final
-- Description : Divisores de un número con final dado.
-- Copyright   : Exercitium (16-06-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Divisores de un número con final dado__
-- 
-- Definir la función
-- 
-- > divisoresConFinal :: Integer -> Integer -> [Integer]
-- 
-- tal que __(divisoresConFinal n m)__ es la lista de los divisores de n
-- cuyos dígitos finales coincide con m. Por ejemplo,
-- 
-- >>> divisoresConFinal 84 4
-- [4,14,84]
-- >>> divisoresConFinal 720 20
-- [20,120,720]

module Divisores_con_final where

import Data.List (isSuffixOf)
import Test.QuickCheck

-- | 1ª definición.
divisoresConFinal :: Integer -> Integer -> [Integer]
divisoresConFinal n m = 
  [x | x <- [1..n]
     , n `rem` x == 0
     , final x m]

-- | __(final x y)__ se verifica si las cifras finales de x coincide con
-- y. Por ejemplo,
-- 
-- >>> final 325 5
-- True
-- >>> final 325 25
-- True
-- >>> final 325 35
-- False
final :: Integer -> Integer -> Bool
final x y = take n xs == ys
  where xs = reverse (show x)
        ys = reverse (show y)
        n  = length ys

-- | 2ª solución (con 'isSuffixOf').
divisoresConFinal2 :: Integer -> Integer -> [Integer]
divisoresConFinal2 n m = 
  [x | x <- [1..n]
     , n `rem` x == 0
     , show m `isSuffixOf` show x]

-- | Comprobación de la equivalencia de las definiciones de
-- 'divisoresConFinal'.
--
-- >>> quickCheck prop_equiv_divisoresConFinal
-- +++ OK, passed 100 tests.
prop_equiv_divisoresConFinal :: (Positive Integer) -> (Positive Integer) -> Bool
prop_equiv_divisoresConFinal (Positive n) (Positive m) =
  divisoresConFinal n m == divisoresConFinal2 n m
