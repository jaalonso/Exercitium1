-- |
-- Module      : MaximosLocales
-- Description : Máximos locales.
-- Copyright   : J.A. Alonso (05-05-14)
-- License     : GPL-3
-- 
-- Un máximo local de una lista es un elemento de la lista que es mayor
-- que su predecesor y que su sucesor en la lista. Por ejemplo, 5 es un
-- máximo local de [3,2,5,3,7,7,1,6,2] ya que es mayor que 2 (su
-- predecesor) y que 3 (su sucesor).
-- 
-- Definir la función
--
-- >   maximosLocales :: Ord a => [a] -> [a]
-- tal que __(maximosLocales xs)__ es la lista de los máximos locales de la
-- lista xs. Por ejemplo,
-- 
-- >>> maximosLocales [3,2,5,3,7,7,1,6,2]
-- [5,6]
-- >>> maximosLocales [1..100]
-- []
-- >>> maximosLocales "adbpmqexyz"
-- "dpq"

module MaximosLocales where

import Test.QuickCheck

-- | 1ª definición (por recursión):
maximosLocales :: Ord a => [a] -> [a]
maximosLocales (x:y:z:xs)
  | y > x && y > z = y : maximosLocales (z:xs)
  | otherwise      = maximosLocales (y:z:xs)
maximosLocales _   = []

-- | 2ª definición (por comprensión):
maximosLocales2 :: Ord a => [a] -> [a]
maximosLocales2 xs = 
  [y | (x,y,z) <- zip3 xs (tail xs) (drop 2 xs), y > x, y > z]

-- | (prop_prop_maximosLocales xs) se verifica si las definiciones de
-- maximosLocales son equivalentes sobre xs. Por ejemplo,
--
-- >>> prop_maximosLocales [3,2,5,3,7,7,1,6,2]
-- True
-- >>> prop_maximosLocales [1..100]
-- True
-- >>> prop_maximosLocales "adbpmqexyz"
-- True
prop_maximosLocales :: Ord a => [a] -> Bool
prop_maximosLocales xs =
  maximosLocales xs == maximosLocales2 xs

-- | Comprueba la equivalencia de las definiciones de maximosLocales.
--
-- >>> verifica_maximosLocales
-- +++ OK, passed 100 tests.
verifica_maximosLocales :: IO ()
verifica_maximosLocales =
  quickCheck (prop_maximosLocales :: [Int] -> Bool)

-- Comprobación
--    stack exec doctest src/MaximosLocales.hs 
--    Examples: 7  Tried: 7  Errors: 0  Failures: 0
