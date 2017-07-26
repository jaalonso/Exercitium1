-- |
-- Module      : ElementosMinimales
-- Description : Determinación de los elementos minimales.
-- Copyright   : Exercitium (24-04-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > minimales :: Eq a => [[a]] -> [[a]]
-- 
-- tal que __(minimales xss)__ es la lista de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
-- 
-- >>> minimales [[1,3],[2,3,1],[3,2,5]]
-- [[2,3,1],[3,2,5]]
-- >>> minimales [[1,3],[2,3,1],[3,2,5],[3,1]]
-- [[2,3,1],[3,2,5]]

module ElementosMinimales where

import Data.List ( delete
                 , nub)

-- | Definición
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

