-- |
-- Module      : Descomposiciones_triangulares
-- Description : Descomposiciones como sumas de tres triangulares.
-- Copyright   : Exercitium (05-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Descomposiciones como sumas de tres triangulares__
--
-- Definir la función
-- 
-- > descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
-- 
-- tal que __(descomposicionesTriangulares n)__ es la lista de las
-- ternas correspondientes a las descomposiciones de n en tres sumandos,
-- como máximo, formados por números triangulares. Por ejemplo,
-- 
-- >>> descomposicionesTriangulares 6
-- [(0,0,6),(0,3,3)]
-- >>> descomposicionesTriangulares 26
-- [(1,10,15),(6,10,10)]
-- >>> descomposicionesTriangulares 21
-- [(0,0,21),(0,6,15),(1,10,10),(3,3,15)]
-- >>> descomposicionesTriangulares 61
-- [(0,6,55),(1,15,45),(3,3,55),(6,10,45),(10,15,36)]
-- >>> descomposicionesTriangulares 66
-- [(0,0,66),(0,21,45),(1,10,55),(6,15,45),(10,28,28),(15,15,36)]
-- >>> descomposicionesTriangulares 126
-- [(0,6,120),(0,21,105),(3,3,120),(3,45,78),(6,15,105),(15,45,66),(36,45,45)]
-- >>> length (descomposicionesTriangulares 9256)
-- 88
-- >>> length (descomposicionesTriangulares 10000)
-- 42
--
-- Comprobar con QuickCheck que todo número se puede descomponer como
-- suma de tres triangulares.

module Descomposiciones_triangulares where

import Test.QuickCheck

-- | Definición.
descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
descomposicionesTriangulares n =         
  [(x,y,n-x-y) | x <- xs
               , y <- dropWhile (<x) xs
               , n-x-y `elem` dropWhile (<y) xs]
  where xs = takeWhile (<=n) triangulares

-- | __triangulares__ es la lista de los números triangulares. Por ejemplo,
--
-- >>> take 10 triangulares
-- [0,1,3,6,10,15,21,28,36,45]
triangulares :: [Int]
triangulares = scanl (+) 0 [1..]

-- | Comprobación de la propiedad.
-- 
-- >>> quickCheck prop_descomposicionesTriangulares
-- +++ OK, passed 100 tests.
prop_descomposicionesTriangulares :: (Positive Int) -> Bool
prop_descomposicionesTriangulares (Positive n) =
  not (null (descomposicionesTriangulares n))
  
