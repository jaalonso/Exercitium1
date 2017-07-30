-- |
-- Module      : Indices_verdaderos
-- Description : Índices de valores verdaderos.
-- Copyright   : Exercitium (04-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Índices de valores verdaderos__
-- 
-- Definir la función
-- 
-- > indicesVerdaderos :: [Int] -> [Bool]
-- 
-- tal que __(indicesVerdaderos xs)__ es la lista infinita de booleanos tal
-- que sólo son verdaderos los elementos cuyos índices pertenecen a la
-- lista estrictamente creciente xs. Por ejemplo,
-- 
-- >>> take 6 (indicesVerdaderos [1,4])
-- [False,True,False,False,True,False]
-- >>> take 6 (indicesVerdaderos [0,2..])
-- [True,False,True,False,True,False]
-- >>> take 3 (indicesVerdaderos [])
-- [False,False,False]
-- >>> take 6 (indicesVerdaderos [1..])
-- [False,True,True,True,True,True]

module Indices_verdaderos where

import Data.List (nub, sort)
import Test.QuickCheck

-- | 1ª definición (por comprensión).
indicesVerdaderos :: [Int] -> [Bool]
indicesVerdaderos xs = [pertenece x xs | x <- [0..]]

-- | __(pertenece x ys)__ se verifica si x pertenece a la lista estrictamente
-- creciente (posiblemente infinita) ys. Por ejemplo,
-- 
-- >>> pertenece 9 [1,3..]
-- True
-- >>> pertenece 6 [1,3..]
-- False
pertenece :: Int -> [Int] -> Bool
pertenece x ys = x `elem` takeWhile (<=x) ys

-- | 2ª solución (por recursión).
indicesVerdaderos2 :: [Int] -> [Bool]
indicesVerdaderos2 []     = repeat False
indicesVerdaderos2 (x:ys) =
  replicate x False ++ [True] ++ indicesVerdaderos2 [y-x-1 | y <- ys]

-- | 3ª solución (por recursión).
indicesVerdaderos3 :: [Int] -> [Bool]
indicesVerdaderos3 xs = aux xs 0 ++ repeat False
  where aux []     _ = []
        aux (y:ys) n | y == n    = True  : aux ys     (n+1) 
                     | otherwise = False : aux (y:ys) (n+1)

-- | 4ª definición (por recursión).
indicesVerdaderos4 :: [Int] -> [Bool]
indicesVerdaderos4 xs = aux xs [0..]
  where aux (y:ys) (i:is) | i == y = True  : aux ys is
                          | y > i  = False : aux (y:ys) is
                          | y < i  = False : aux ys is
        aux _ _                    = repeat False

-- | Tipo de los índices.
newtype Indices = I [Int]
  deriving Show

-- | Generador de índices. Por ejemplo,
-- 
-- > > sample indices
-- > I []
-- > I [1]
-- > I []
-- > I [2]
-- > I [0,1,5,8]
-- > I [4,6,7,10]
-- > I [0,11]
-- > I [1,5,6,9,10]
-- > I [0,7,8,10,12,14,16]
-- > I [0,5,13]
-- > I [17]
indices :: Gen Indices
indices = do
  xs <- arbitrary
  return (I (dropWhile (< 0) (sort (nub xs))))

-- | Inclusión de 'Indices' en 'Arbitrary'.
instance Arbitrary Indices where
  arbitrary = indices

-- | Comprobación de la equivalencia de las definiciones de
-- 'indicesVerdaderos'.
-- 
-- >>> quickCheck prop_equiv_indicesVerdaderos
-- +++ OK, passed 100 tests.
prop_equiv_indicesVerdaderos :: (Positive Int) -> Indices -> Bool
prop_equiv_indicesVerdaderos (Positive n) (I xs) =
  all (== take n (indicesVerdaderos xs))
      [take n (f xs) | f <- [ indicesVerdaderos2
                            , indicesVerdaderos3
                            , indicesVerdaderos4
                            ]]
