-- |
-- Module      : Primos_equidistantes
-- Description : Pares de primos consecutivos a distancia dada.
-- Copyright   : Exercitium (30-04-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > primosEquidistantes :: Integer -> [(Integer,Integer)]
--
-- tal que __(primosEquidistantes k)__ es la lista de los pares de primos
-- cuya diferencia es k. Por ejemplo,
--
-- >>> take 3 (primosEquidistantes 2)
-- [(3,5),(5,7),(11,13)]
-- >>> take 3 (primosEquidistantes 4)
-- [(7,11),(13,17),(19,23)]
-- >>> take 3 (primosEquidistantes 6)
-- [(23,29),(31,37),(47,53)]
-- >>> take 3 (primosEquidistantes 8)
-- [(89,97),(359,367),(389,397)]

module Primos_equidistantes
  ( primosEquidistantes
  , primosEquidistantes2
  ) where

import Data.Numbers.Primes (primes)

-- | 1ª definición
primosEquidistantes :: Integer -> [(Integer,Integer)]
primosEquidistantes k = aux primos
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)
        aux _                     = []               

-- | (primo x) se verifica si x es primo. Por ejemplo,
--
-- >>> primo 7
-- True
-- >>> primo 8
-- False
primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- | primos es la lista de los números primos. Por ejemplo,
-- 
-- take 10 primos
-- [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]

-- | 2ª definición
-- 
-- >>> take 20 (primosEquidistantes2 2) == take 20 (primosEquidistantes 2)
-- True
-- >>> take 20 (primosEquidistantes2 4) == take 20 (primosEquidistantes 4)
-- True
primosEquidistantes2 :: Integer -> [(Integer,Integer)]
primosEquidistantes2 k = aux primes
  where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                     | otherwise  = aux (y:ps)
        aux _                     = []               

-- Comparación de eficiencia
--    λ> (primosEquidistantes 10) !! 150
--    (13513,13523)
--    (2.18 secs, 1,325,730,640 bytes)
--    λ> (primosEquidistantes2 10) !! 150
--    (13513,13523)
--    (0.01 secs, 0 bytes)

-- Comprobación
--    > stack exec doctest src/Primos_equidistantes.hs 
--    Examples: 8  Tried: 8  Errors: 0  Failures: 0
