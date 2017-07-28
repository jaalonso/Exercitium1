-- |
-- Module      : Regiones
-- Description : Regiones en el plano.
-- Copyright   : Exercitium (19-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- En los siguientes dibujos se observa que el número máximo de regiones
-- en el plano generadas con 1, 2 ó 3 líneas son 2, 4 ó 7,
-- respectivamente.
--
-- >
-- >                      \  |
-- >                       \5|
-- >                        \|
-- >                         \
-- >                         |\
-- >                         | \
-- >               |         |  \ 
-- >    1        1 | 3     1 | 3 \  6
-- >   ------   ---|---   ---|----\---
-- >    2        2 | 4     2 | 4   \ 7
-- >               |         |      \
-- 
-- Definir la función
-- 
-- > regiones :: Integer -> Integer  
-- tal que __(regiones n)__ es el número máximo de regiones en el plano
-- generadas con n líneas. Por ejemplo,
--
-- >>> regiones 3
-- 7  
-- >>> regiones 100
-- 5051

module Regiones where

import Test.QuickCheck

-- | 1ª definición (por recursión).
regiones :: Integer -> Integer  
regiones 0 = 1
regiones n = regiones (n-1) + n  

-- | 2ª definición (por la fórmula).
regiones2 :: Integer -> Integer
regiones2 n = n*(n+1) `div` 2 + 1

-- | __(prop_regiones n)__ se verifica si las definiciones de regiones
-- son equivalentes sobre n. Por ejemplo,
--
-- >>> all prop_regiones [Positive 2, Positive 100]
-- True
-- >>> quickCheck prop_regiones
-- +++ OK, passed 100 tests.
prop_regiones :: (Positive Integer) -> Bool
prop_regiones (Positive n) =
  regiones n == regiones2 n
  
