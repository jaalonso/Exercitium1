-- |
-- Module      : Numero_de_inversiones
-- Description : Número de inversiones.
-- Copyright   : Exercitium 
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Número de inversiones__
-- 
-- Se dice que en una sucesión de números x(1), x(2), ..., x(n) hay una 
-- inversión cuando existe un par de números x(i) > x(j), siendo i < j.
-- Por ejemplo, en la permutación 2, 1, 4, 3 hay dos inversiones 
-- (2 antes que 1 y 4 antes que 3) y en la permutación 4, 3, 1, 2 hay 
-- cinco inversiones (4 antes 3, 4 antes 1, 4 antes 2, 3 antes 1, 
-- 3 antes 2).
-- 
-- Definir la función
-- 
-- > numeroInversiones :: Ord a => [a] -> Int  
-- 
-- tal que __(numeroInversiones xs)__ es el número de inversiones de xs. Por
-- ejemplo, 
-- 
-- >>> numeroInversiones [2,1,4,3]
-- 2
-- >>> numeroInversiones [4,3,1,2]
-- 5

module Numero_de_inversiones where

import Data.Array
import Test.QuickCheck

-- | 1ª solución (por recursión y compresión).
numeroInversiones :: Ord a => [a] -> Int  
numeroInversiones [] = 0
numeroInversiones (x:xs) =
  length [y | y <- xs, x > y] + numeroInversiones xs

-- | 2ª solución (por recursión y 'filter').
numeroInversiones2 :: Ord a => [a] -> Int  
numeroInversiones2 [] = 0
numeroInversiones2 (x:xs) =
  length (filter (x>) xs) + numeroInversiones2 xs

-- | 3ª solución (por comprensión)
numeroInversiones3 :: Ord a => [a] -> Int  
numeroInversiones3 xs =
  length [(i,j) | i <- [0..n-2]
                , j <- [i+1..n-1]
                , xs!!i > xs!!j]
  where n = length xs

-- | 4ª solución (con vectores)
numeroInversiones4 :: Ord a => [a] -> Int  
numeroInversiones4 xs =
  length [(i,j) | i <- [1..n-1]
                , j <- [i+1..n]
                , v!i > v!j]
  where n = length xs
        v = listArray (1,n) xs

-- | __(prop_equiv_numeroInversiones xs)__ se verifica si las
-- definiciones de 'numeroInversiones' son equivalentes sobre xs. Por
-- ejemplo,
--
-- >>> all prop_equiv_numeroInversiones [[2,1,4,3], [4,3,1,2]]
-- True
prop_equiv_numeroInversiones :: [Int] -> Bool
prop_equiv_numeroInversiones xs =
  all (== numeroInversiones xs)
      [f xs | f <- [ numeroInversiones2
                   , numeroInversiones3
                   , numeroInversiones4
                   ]]

-- | Comprueba la equivalencia de las definiciones de
-- 'numeroInversiones'. 
-- 
-- >>> verifica_equiv_numeroInversiones
-- +++ OK, passed 100 tests.
verifica_equiv_numeroInversiones :: IO ()
verifica_equiv_numeroInversiones =
  quickCheck prop_equiv_numeroInversiones

-- $doc
--
-- __Comparación de eficiencia__
--
-- > > numeroInversiones [2000,1999..1]
-- > 1999000
-- > (1.94 secs, 257,172,224 bytes)
-- > > numeroInversiones2 [2000,1999..1]
-- > 1999000
-- > (0.20 secs, 113,228,368 bytes)
-- > > numeroInversiones3 [2000,1999..1]
-- > 1999000
-- > (27.40 secs, 672,913,536 bytes)
-- > > numeroInversiones4 [2000,1999..1]
-- > 1999000
-- > (4.18 secs, 832,705,200 bytes)
