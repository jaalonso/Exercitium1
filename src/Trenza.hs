-- |
-- Module      : Trenza
-- Description : Trenzado de listas.
-- Copyright   : Exercitium (26-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
--
-- __Trenzado de listas__
-- 
-- Definir la función
-- 
-- > trenza :: [a] -> [a] -> [a]
-- 
-- tal que __(trenza xs ys)__ es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
-- 
-- >>> trenza [5,1] [2,7,4]
-- [5,2,1,7]
-- >>> trenza [5,1,7] [2..]
-- [5,2,1,3,7,4]
-- >>> trenza [2..] [5,1,7]
-- [2,5,3,1,4,7]
-- >>> take 8 (trenza [2,4..] [1,5..])
-- [2,1,4,5,6,9,8,13]

module Trenza where

import Test.QuickCheck

-- | 1ª definición (por comprensión).
trenza :: [a] -> [a] -> [a]
trenza xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- | 2ª definición (con 'zipWith').
trenza2 :: [a] -> [a] -> [a]
trenza2 xs ys = concat (zipWith par xs ys)
  where par x y = [x,y]

-- | 3ª definición (con 'zipWith' y sin argumentos).
trenza3 :: [a] -> [a] -> [a]
trenza3 = (concat .) . zipWith par
  where par x y = [x,y]

-- | 4ª definición (por recursión).
trenza4 :: [a] -> [a] -> [a]
trenza4 (x:xs) (y:ys) = x : y : trenza xs ys
trenza4 _      _      = []

-- | __prop_trenza xs ys)__ se verifica si las definiciones de 'trenza'
-- son equivalentes para xs e ys. Por ejemplo,
--
-- >>> :{
--  and [ prop_trenza [5,1] [2,7,4]
--      , prop_trenza [5,1,7] [2..]
--      , prop_trenza [2..] [5,1,7] ]
-- :}
-- True
prop_trenza :: Eq a => [a] -> [a] -> Bool
prop_trenza xs ys =
  all (== trenza xs ys)
      [f xs ys | f <- [ trenza2
                      , trenza3
                      , trenza4
                      ]]

-- | Comprueba la equivalencia de las definiciones de 'trenza'.
-- 
-- >>> verifica_trenza
-- +++ OK, passed 100 tests.
verifica_trenza :: IO ()
verifica_trenza =
  quickCheck (prop_trenza :: [Int] -> [Int] -> Bool)
