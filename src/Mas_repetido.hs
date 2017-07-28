-- |
-- Module      : Mas_repetido
-- Description : Elemento con más repeticiones consecutivas.
-- Copyright   : Exercitium (20-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > masRepetido :: Ord a => [a] -> (a,Int)
--
-- tal que __(masRepetido xs)__ es el mayor elemento de xs que aparece
-- más veces de manera consecutiva en la lista junto con el número de sus
-- apariciones consecutivas. Por ejemplo,
-- 
-- >>> masRepetido [1,1,4,4,1]
-- (4,2)
-- >>> masRepetido [4,4,1,1,4]
-- (4,2)
-- >>> masRepetido "aadda"
-- ('d',2)
-- >>> masRepetido "a"
-- ('a',1)
-- >>> masRepetido "ba"
-- ('b',1)

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mas_repetido where

import Data.List (group, maximumBy)
import Data.Function (on)
import Data.Tuple (swap)
import Control.Arrow ((&&&))
import Test.QuickCheck

-- | 1ª definición (por recursión)
masRepetido :: Ord a => [a] -> (a,Int)
masRepetido = maximum . masRepetidos

-- | __(masRepetidos xs)__ es la lista de los elementos de xs con más
-- repeticiones consecutivas junto con el número de sus repeticiones
-- consecutivas. Por ejemplo,
--
-- >>> masRepetidos "aabxxbccb"
-- [('a',2),('x',2),('c',2)]
masRepetidos :: Ord a => [a] -> [(a,Int)]
masRepetidos []  = []
masRepetidos [x] = [(x,1)]
masRepetidos (x:y:zs) | m > n     = [(x,m)]
                      | m == n    = (x,m):vs
                      | otherwise = vs
  where vs@((_,n):_) = masRepetidos (y:zs)
        m            = length (takeWhile (==x) (x:y:zs))

-- | 2ª definición (con 'group' y 'maximum').
masRepetido2 :: Ord a => [a] -> (a,Int)
masRepetido2 xs = (n,z)
  where (z,n) = maximum [(1 + length ys,y) | (y:ys) <- group xs]
  
-- | 3ª definición (con 'group', 'maximum' y 'swap').
masRepetido3 :: Ord a => [a] -> (a,Int)
masRepetido3 xs = 
  swap (maximum [(1 + length ys,y) | (y:ys) <- group xs])

-- | 4ª definición (con 'group' y 'maximumBy').
masRepetido4 :: Ord a => [a] -> (a,Int)
masRepetido4 xs =  
  maximumBy compara [(y,1 + length ys) | (y:ys) <- group xs]
  where compara (a,n) (b,m) = compare (n,a) (m,b)

-- | 5ª definición (sin argumentos)
masRepetido5 :: Ord a => [a] -> (a,Int)
masRepetido5 =
  maximumBy (compare `on` (snd &&& fst)) . map (head &&& length) . group

-- | __(prop_masRepetido xs)__ se verifica si las definiciones de
-- masRepetido son equivalentes sobre xs. Por ejemplo,
-- 
-- >>> quickCheck prop_masRepetido
-- +++ OK, passed 100 tests.
prop_masRepetido :: (NonEmptyList Int) -> Bool
prop_masRepetido (NonEmpty xs) =
  all (== masRepetido xs)
      [f xs | f <- [ masRepetido2
                   , masRepetido3
                   , masRepetido4
                   , masRepetido5
                   ]]
