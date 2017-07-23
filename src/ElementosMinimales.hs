-- |
-- Module      : ElementosMinimales
-- Description : Determinación de los elementos minimales.
-- Copyright   : José A. Alonso (24 de Abril de 2014)
-- License     : GPL-3
-- 
-- Definir la función
-- 
-- > minimales :: Eq a => [[a]] -> [[a]]
-- 
-- tal que (minimales xss) es la lista de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
-- 
-- >>> minimales [[1,3],[2,3,1],[3,2,5]]
-- [[2,3,1],[3,2,5]]
-- >>> minimales [[1,3],[2,3,1],[3,2,5],[3,1]]
-- [[2,3,1],[3,2,5]]

module ElementosMinimales
  ( minimales
  ) where

import Data.List ( delete
                 , nub)

minimales :: Eq a => [[a]] -> [[a]]
minimales xss = 
  [xs | xs <- xss
      , null [ys | ys <- xss, subconjuntoPropio xs ys]]

-- | (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo,
-- 
-- >>> subconjuntoPropio [1,3] [3,1,3]
-- False
-- >>> subconjuntoPropio [1,3,1] [3,1,2]
-- True
subconjuntoPropio :: Eq a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = aux (nub xs) (nub ys)
  where
    aux _ []      = False
    aux [] _      = True
    aux (a:as) bs = a `elem` bs && aux as (delete a bs)  

-- Comprobación
--    > stack exec doctest src/ElementosMinimales.hs 
--    Examples: 4  Tried: 4  Errors: 0  Failures: 0
