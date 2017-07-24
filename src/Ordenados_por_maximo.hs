-- |
-- Module      : Ordenados_por_maximo.hs
-- Description : Ordenación por el máximo.
-- Copyright   : José A. Alonso (22-04-14)
-- License     : GPL-3
--
-- Definir la función
--
-- > ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
-- 
-- tal que __(ordenadosPorMaximo xss)__ es la lista de los elementos de xss
-- ordenada por sus máximos. Por ejemplo,
-- 
-- >>> ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
-- [[3,2],[1,4],[6,7,5]]
-- >>> ordenadosPorMaximo ["este","es","el","primero"]
-- ["el","primero","es","este"]

module Ordenados_por_maximo
  ( ordenadosPorMaximo
  , verificaOrdenadosPorMaximo
  ) where

import Data.List (sort)
import Test.QuickCheck

ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo xss =
  map snd (sort [(maximum xs,xs) | xs <- xss])

-- | 2ª definición
ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
  [xs | (_,xs) <- sort [(maximum xs,xs) | xs <- xss]]

-- | (prop_ordenadosPorMaximo xs) se verifica si todas las definiciones
-- de ordenadosPorMaximo son equivalentes para xs. Por ejemplo,
--
-- >>> prop_ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
-- True
-- >>> prop_ordenadosPorMaximo ["este","es","el","primero"]
-- True
prop_ordenadosPorMaximo :: Ord a => [[a]] -> Bool
prop_ordenadosPorMaximo xs =
  ordenadosPorMaximo ys == ordenadosPorMaximo2 ys
  where ys = filter (not . null) xs

-- | Comprueba la equivalencia de las definiciones
--
-- >>> verificaOrdenadosPorMaximo
-- +++ OK, passed 100 tests.
verificaOrdenadosPorMaximo :: IO ()
verificaOrdenadosPorMaximo = 
  quickCheck (prop_ordenadosPorMaximo :: [[Int]] -> Bool)

-- Comprobación
--    > stack exec doctest src/Iguales_al_siguiente.hs 
--    Examples: 5  Tried: 5  Errors: 0  Failures: 0
