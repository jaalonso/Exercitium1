-- |
-- Module      : Iguales_al_siguiente.hs
-- Description : Elementos iguales a su siguiente.
-- Copyright   : Exercitium (21-04-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > igualesAlSiguiente :: Eq a => [a] -> [a]
--
-- tal que __(igualesAlSiguiente xs)__ es la lista de los elementos de xs
-- que son iguales a su siguiente. Por ejemplo,
--
-- >>> igualesAlSiguiente [1,2,2,2,3,3,4::Int]
-- [2,2,3]
-- >>> igualesAlSiguiente [1..10::Int]
-- []

module Iguales_al_siguiente where

import Data.List (group)
import Test.QuickCheck

-- | 1ª definición (con zip)
igualesAlSiguiente :: Eq a => [a] -> [a]
igualesAlSiguiente xs =
  [x | (x,y) <- zip xs (tail xs), x == y]

-- | 2ª definición (por recursión)
igualesAlSiguiente2 :: Eq a => [a] -> [a]
igualesAlSiguiente2 (x:y:zs)
  | x == y    = x : igualesAlSiguiente2 (y:zs)
  | otherwise = igualesAlSiguiente2 (y:zs)
igualesAlSiguiente2 _ = []

-- | 3ª definición (con concat y comprensión)
igualesAlSiguiente3 :: Eq a => [a] -> [a]
igualesAlSiguiente3 xs = concat [ys | (_:ys) <- group xs]

-- | 4ª definición (con concatMap)
igualesAlSiguiente4 :: Eq a => [a] -> [a]
igualesAlSiguiente4 xs = concatMap tail (group xs)

-- | 5ª definición (con concatMap y sin argumentos):
igualesAlSiguiente5 :: Eq a => [a] -> [a]
igualesAlSiguiente5 = concatMap tail . group

-- | (prop_igualesAlSiguiente xs) se verifica si todas las definiciones
-- de igualesAlsiguiente son equivalentes para xs. Por ejemplo,
--
-- >>> prop_igualesAlSiguiente [1,2,2,2,3,3,4]
-- True
-- >>> prop_igualesAlSiguiente [1..10]
-- True
prop_igualesAlSiguiente :: [Int] -> Bool
prop_igualesAlSiguiente xs =
  all (== igualesAlSiguiente xs)
      [f xs | f <- [ igualesAlSiguiente2
                   , igualesAlSiguiente3
                   , igualesAlSiguiente4
                   , igualesAlSiguiente5]]

-- | Comprueba la equivalencia de las definiciones
--
-- > verificaIgualesAlSiguiente
-- +++ OK, passed 100 tests.
verifica_igualesAlSiguiente :: IO ()
verifica_igualesAlSiguiente = 
  quickCheck prop_igualesAlSiguiente



