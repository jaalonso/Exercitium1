-- |
-- Module      : Segmentos_consecutivos.hs
-- Description : Segmentos de elementos consecutivos.
-- Copyright   : J.A. Alonso (07-05-14)
-- License     : GPL-3
-- 
-- Definir la función
-- 
-- > segmentos :: (Enum a, Eq a) => [a] -> [[a]]
-- 
-- tal que __(segmentos xss)__ es la lista de los segmentos de xss formados
-- por elementos consecutivos. Por ejemplo,
-- 
-- >>> segmentos [1,2,5,6,4]
-- [[1,2],[5,6],[4]]
-- >>> segmentos [1,2,3,4,7,8,9]
-- [[1,2,3,4],[7,8,9]]
-- >>> segmentos "abbccddeeebc"
-- ["ab","bc","cd","de","e","e","bc"]
--
-- Nota: Se puede usar la función succ tal que (succ x) es el sucesor de
-- x. Por ejemplo,
--
-- > succ 3    ==  4
-- > succ 'c'  ==  'd'

module Segmentos_consecutivos where

import Test.QuickCheck

-- | 1ª definición (por recursión).
segmentos :: (Enum a, Eq a) => [a] -> [[a]]
segmentos []  = []
segmentos [x] = [[x]]
segmentos (x:xs) | y == succ x = (x:y:ys):zs
                 | otherwise   = [x]:(y:ys):zs
    where ((y:ys):zs) = segmentos xs

-- | 2ª definición.
segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
  where ys = inicial xs
        n  = length ys
        zs = drop n xs

-- | (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
-- 
-- >>> inicial [1,2,5,6,4]
-- [1,2]
-- >>> inicial "abccddeeebc"
-- "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial [] = []
inicial (x:xs) = 
  [y | (y,_) <- takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..])]

-- | 2ª definición de inicial (con uncurry)
-- 
-- >>> inicial2 [1,2,5,6,4]
-- [1,2]
-- >>> inicial2 "abccddeeebc"
-- "abc"
inicial2 :: (Enum a, Eq a) => [a] -> [a]
inicial2 [] = []
inicial2 (x:xs) = 
  [y | (y,_) <- takeWhile (uncurry (==)) (zip (x:xs) [x..])]

-- | (prop_segmentos xs) se verifica si las definiciones de segmentos
-- son equivalentes sobre xs. Por ejemplo,
--   
-- >>> prop_segmentos [1,2,5,6,4]
-- True
-- >>> prop_segmentos [1,2,3,4,7,8,9]
-- True
-- >>> prop_segmentos "abbccddeeebc"
-- True
prop_segmentos :: (Enum a, Eq a) => [a] -> Bool
prop_segmentos xs =
  segmentos xs == segmentos2 xs

-- | Comprueba la equivalencia de las definiciones de segmentos.
-- 
--    >>> verifica_segmentos
--    +++ OK, passed 100 tests.
verifica_segmentos :: IO ()
verifica_segmentos =
  quickCheck (prop_segmentos :: [Int] -> Bool)
