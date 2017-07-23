-- |
-- Module      : Ordenados_por_maximo.hs
-- Description : Ordenación por el máximo.
-- Copyright   : José A. Alonso (22 de Abril de 2014)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- Stability   : Provisional
--
-- 
-- Definir la función
--
-- > ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
-- 
-- tal que (ordenadosPorMaximo xss) es la lista de los elementos de xss
-- ordenada por sus máximos. Por ejemplo,
-- 
-- >>> ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
-- [[3,2],[1,4],[6,7,5]]
-- >>> ordenadosPorMaximo ["este","es","el","primero"]
-- ["el","primero","es","este"]

module Ordenados_por_maximo where

import Data.List (sort)
import GHC.Exts  (sortWith)

-- 1ª definición
ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo xss =
  map snd (sort [(maximum xs,xs) | xs <- xss])

-- 2ª definición
ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
  [xs | (_,xs) <- sort [(maximum xs,xs) | xs <- xss]]

-- 3ª definición:
ordenadosPorMaximo3 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo3 = sortWith maximum

