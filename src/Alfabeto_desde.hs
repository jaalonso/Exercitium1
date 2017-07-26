-- |
-- Module      : Alfabeto_desde
-- Description : Alfabeto comenzando en un carácter.
-- Copyright   : Exercitium (12-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > alfabetoDesde :: Char -> String
-- 
-- tal que __(alfabetoDesde c)__ es el alfabeto, en minúscula, comenzando en
-- el carácter c, si c es una letra minúscula y comenzando en 'a', en
-- caso contrario. Por ejemplo,
-- 
-- >>> alfabetoDesde 'e'
-- "efghijklmnopqrstuvwxyzabcd"
-- >>> alfabetoDesde 'a'
-- "abcdefghijklmnopqrstuvwxyz"
-- >>> alfabetoDesde '7'
-- "abcdefghijklmnopqrstuvwxyz"
-- >>> alfabetoDesde '{'
-- "abcdefghijklmnopqrstuvwxyz"
-- >>> alfabetoDesde 'B'
-- "abcdefghijklmnopqrstuvwxyz"

module Alfabeto_desde where

import Data.Char  (isAsciiLower)
import Data.Tuple (swap)
import Test.QuickCheck

-- | 1ª definición (con 'dropWhile' y 'takeWhile').
alfabetoDesde :: Char -> String
alfabetoDesde c =
  dropWhile (<c) ['a'..'z'] ++ takeWhile (<c) ['a'..'z']

-- | 2ª definición (con 'span').
alfabetoDesde2 :: Char -> String
alfabetoDesde2 c = ys ++ xs
  where (xs,ys) = span (<c) ['a'..'z']

-- | 3ª definición (con 'break').
alfabetoDesde3 :: Char -> String
alfabetoDesde3 c = ys ++ xs
  where (xs,ys) = break (==c) ['a'..'z']

-- | 4ª definición (sin argumentos):
alfabetoDesde4 :: Char -> String
alfabetoDesde4 =
  uncurry (++) . swap . flip span ['a'..'z'] . (>)

-- Ejemplo de cálculo:
--    alfabetoDesde4 'e'
--    = (uncurry (++) . swap . flip span ['a'..'z'] . (>)) 'e'
--    = (uncurry (++) . swap) ("abcd","efghijklmnopqrstuvwxyz")
--    = uncurry (++) ("efghijklmnopqrstuvwxyz","abcd")
--    = (++) "efghijklmnopqrstuvwxyz" "abcd"
--    = "efghijklmnopqrstuvwxyzabcd"

-- | 5ª definición (sin argumentos).
alfabetoDesde5 :: Char -> String
alfabetoDesde5 =
  uncurry (flip (++)) . (`break` ['a'..'z']) . (==)

-- Ejemplo de cálculo:
--    alfabetoDesde5 'e'
--    = (uncurry (flip (++)) . (`break` ['a'..'z']) . (==)) 'e'
--    = uncurry (flip (++)) ("abcd","efghijklmnopqrstuvwxyz")
--    = flip (++) "abcd" "efghijklmnopqrstuvwxyz"
--    = "efghijklmnopqrstuvwxyz" ++ "abcd"
--    = "efghijklmnopqrstuvwxyzabcd"

-- | 6ª definición (por comprensión).
alfabetoDesde6 :: Char -> String
alfabetoDesde6 c
    | c >= 'a' && c <= 'z' = [c..'z'] ++ ['a'..pred c]
    | otherwise            = ['a'..'z']

-- | 7ª definición (por comprensión con 'isAsciiLower').
alfabetoDesde7 :: Char -> String
alfabetoDesde7 c
    | isAsciiLower c = [c..'z'] ++ ['a'..pred c]
    | otherwise      = ['a'..'z']

-- | (prop_alfabetoDesde c) se verifica si las definiciones de
-- alfabetoDesde sobre c. Por ejemplo,
--
-- >>> :{
--  and [ prop_alfabetoDesde 'e'
--      , prop_alfabetoDesde 'a'
--      , prop_alfabetoDesde '7'
--      , prop_alfabetoDesde '{'
--      , prop_alfabetoDesde 'B'
--      ]
-- :}
-- True
prop_alfabetoDesde :: Char -> Bool
prop_alfabetoDesde c =
  all (== alfabetoDesde c)
      [f c | f <- [ alfabetoDesde2
                  , alfabetoDesde3
                  , alfabetoDesde4
                  , alfabetoDesde5
                  , alfabetoDesde6
                  , alfabetoDesde7
                  ]]

-- | Comprueba la equivalencia de las definiciones.
-- 
-- >>> verifica_alfabetoDesde
-- +++ OK, passed 100 tests.
verifica_alfabetoDesde :: IO ()
verifica_alfabetoDesde =
  quickCheck prop_alfabetoDesde

