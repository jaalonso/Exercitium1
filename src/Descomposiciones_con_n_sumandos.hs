-- |
-- Module      : Descomposiciones_con_n_sumandos
-- Description : Descomposiciones de x como sumas de n sumandos en la lista ns.
-- Copyright   : Exercitium  (17-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Descomposiciones de x como sumas de n sumandos en la lista ns__
-- 
-- Definir la función
-- 
-- > sumas :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
-- 
-- tal que __(sumas n ns x)__ es la lista de las descomposiciones de x como
-- sumas de n sumandos en la lista ns. Por ejemplo,
-- 
-- >>> sumas 2 [1,2] 3
-- [[1,2]]
-- >>> sumas 2 [-1] (-2)
-- [[-1,-1]]
-- >>> sumas 2 [-1,3,-1] 2
-- [[-1,3]]
-- >>> sumas 2 [1,2] 4
-- [[2,2]]
-- >>> sumas 2 [1,2] 5
-- []
-- >>> sumas 3 [1,2] 5
-- [[1,2,2]]
-- >>> sumas 3 [1,2] 6
-- [[2,2,2]]
-- >>> sumas 2 [1,2,5] 6
-- [[1,5]]
-- >>> sumas 2 [1,2,3,5] 4
-- [[1,3],[2,2]]
-- >>> sumas 2 [1..5] 6
-- [[1,5],[2,4],[3,3]]
-- >>> sumas 3 [1..5] 7
-- [[1,1,5],[1,2,4],[1,3,3],[2,2,3]]
-- >>> sumas 3 [1..200] 4
-- [[1,1,2]]

module Descomposiciones_con_n_sumandos where

import Data.List (nub, sort)
import Test.QuickCheck

-- | 1ª definición (fuerza bruta)
sumas1 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas1 n ns x = 
  [xs | xs <- combinacionesR n (nub (sort ns))
      , sum xs == x]

-- | __(combinacionesR k xs)__ es la lista de las combinaciones orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
-- 
-- >>> combinacionesR 2 "abc"
-- ["aa","ab","ac","bb","bc","cc"]
-- >>> combinacionesR 3 "bc"
-- ["bbb","bbc","bcc","ccc"]
-- >>> combinacionesR 3 "abc"
-- ["aaa","aab","aac","abb","abc","acc","bbb","bbc","bcc","ccc"]
combinacionesR :: Int -> [a] -> [[a]]
combinacionesR _ [] = []
combinacionesR 0 _  = [[]]
combinacionesR k (x:xs) =
  [x:ys | ys <- combinacionesR (k-1) (x:xs)] ++ combinacionesR k xs

-- | 2ª definición (divide y vencerás).
sumas :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas n ns x = nub (sumasAux n ns x)
  where sumasAux :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
        sumasAux 1 ns' x'
          | x' `elem` ns' = [[x']]
          | otherwise   = []
        sumasAux n' ns' x' = 
          concat [[y:zs | zs <- sumasAux (n'-1) ns' (x'-y)
                        , y <= head zs]
                 | y <- ns']

-- | __(prop_equiv_sumas n ns x)__ se verifica si las definiciones de
-- 'sumas' son equivalentes para n, ns y x. Por ejemplo,
-- 
-- >>> quickCheckWith (stdArgs {maxSize=7}) prop_equiv_sumas
-- +++ OK, passed 100 tests.
prop_equiv_sumas :: (Positive Int) -> [Int] -> Int -> Bool
prop_equiv_sumas (Positive n) ns x =
  normal (sumas1 n ns x) == normal (sumas n ns x)
  where normal = sort . map sort

-- $doc
--
-- __Comparación de eficiencia__
-- 
-- > > sumas1 3 [1..200] 4
-- > [[1,1,2]]
-- > (2.52 secs, 1,914,773,472 bytes)
-- > > sumas 3 [1..200] 4
-- > [[1,1,2]]
-- > (0.17 secs, 25,189,688 bytes)
