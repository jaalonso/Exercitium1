-- |
-- Module      : Elimina_unitarias
-- Description : Eliminación de las ocurrencias unitarias.
-- Copyright   : Exercitium (11-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Eliminación de las ocurrencias unitarias__
-- 
-- Definir la función
-- 
-- > eliminaUnitarias :: Char -> String -> String
--
-- tal que __(eliminaUnitarias c cs)__ es la lista obtenida eliminando de la
-- cadena cs las ocurrencias unitarias del carácter c (es decir,
-- aquellas ocurrencias de c tales que su elemento anterior y posterior
-- es distinto de c). Por ejemplo,
-- 
-- >>> eliminaUnitarias 'X' ""
-- ""
-- >>> eliminaUnitarias 'X' "X"
-- ""
-- >>> eliminaUnitarias 'X' "XX"
-- "XX"
-- >>> eliminaUnitarias 'X' "XXX"
-- "XXX"
-- >>> eliminaUnitarias 'X' "abcd"
-- "abcd"
-- >>> eliminaUnitarias 'X' "Xabcd"
-- "abcd"
-- >>> eliminaUnitarias 'X' "XXabcd"
-- "XXabcd"
-- >>> eliminaUnitarias 'X' "XXXabcd"
-- "XXXabcd"
-- >>> eliminaUnitarias 'X' "abcdX"
-- "abcd"
-- >>> eliminaUnitarias 'X' "abcdXX"
-- "abcdXX"
-- >>> eliminaUnitarias 'X' "abcdXXX"
-- "abcdXXX"
-- >>> eliminaUnitarias 'X' "abXcd"
-- "abcd"
-- >>> eliminaUnitarias 'X' "abXXcd"
-- "abXXcd"
-- >>> eliminaUnitarias 'X' "abXXXcd"
-- "abXXXcd"
-- >>> eliminaUnitarias 'X' "XabXcdX"
-- "abcd"
-- >>> eliminaUnitarias 'X' "XXabXXcdXX"
-- "XXabXXcdXX"
-- >>> eliminaUnitarias 'X' "XXXabXXXcdXXX"
-- "XXXabXXXcdXXX"
-- >>> eliminaUnitarias 'X' "XabXXcdXeXXXfXx"
-- "abXXcdeXXXfx"

module Elimina_unitarias where

import Data.List (group)
import Test.QuickCheck

-- | 1ª solución (por comprensión).
eliminaUnitarias :: Char -> String -> String
eliminaUnitarias c cs =
  concat [xs | xs <- group cs, xs /= [c]] 

-- | 2ª solución (por composición).
eliminaUnitarias2 :: Char -> String -> String
eliminaUnitarias2 c =
  concat . filter (/=[c]) . group

-- | 3ª solución (por recursión).
eliminaUnitarias3 :: Char -> String -> String
eliminaUnitarias3 _ [] = []
eliminaUnitarias3 c [x] | c == x    = []
                        | otherwise = [x]
eliminaUnitarias3 c (x:y:zs) 
  | x /= c    = x : eliminaUnitarias3 c (y:zs)
  | y /= c    = y : eliminaUnitarias3 c zs
  | otherwise = takeWhile (==c) (x:y:zs) ++ 
                eliminaUnitarias3 c (dropWhile (==c) zs)

-- | 4ª solución (por recursión con acumuladores).
eliminaUnitarias4 :: Char -> String -> String
eliminaUnitarias4 c cs = reverse (aux0 cs "")
  where aux0 [] cs2                  = cs2
        aux0 (x:cs1) cs2 | x == c    = aux1 cs1 cs2
                         | otherwise = aux0 cs1 (x:cs2)
        aux1 [] cs2                  = cs2
        aux1 (x:cs1) cs2 | x == c    = aux2 cs1 (c:c:cs2)
                         | otherwise = aux0 cs1 (x:cs2)
        aux2 [] cs2                  = cs2
        aux2 (x:cs1) cs2 | x == c    = aux2 cs1 (c:cs2)
                         | otherwise = aux0 cs1 (x:cs2)

-- | 5ª solución (con índices).
eliminaUnitarias5 :: Char -> String -> String
eliminaUnitarias5 c cs = 
  [x | i <- [0..length cs - 1]
     , let x = cs!!i
     , x /= c || ds!!i == c || ds!!(i+2) == c]
  where d  = if c == 'a' then 'b' else 'a'
        ds = d : cs ++ [d]

-- | 6ª solución (por recursión con 'span').
eliminaUnitarias6 :: Char -> String -> String
eliminaUnitarias6 _ [] = []
eliminaUnitarias6 c cs | ys == [c] = xs ++ eliminaUnitarias6 c zs
                       | otherwise = xs ++ ys ++ eliminaUnitarias6 c zs
  where (xs,us) = span (/=c) cs
        (ys,zs) = span (==c) us

-- | __(prop_equiv_eliminaUnitarias c cs)__ se verifica si las
-- definiciones de eliminaUnitarias son equivalentes sobre c y cs. Por
-- ejemplo,
-- 
-- >>> prop_equiv_eliminaUnitarias 'X' "XabXXcdXeXXXfXx"
-- True
prop_equiv_eliminaUnitarias :: Char -> String -> Bool
prop_equiv_eliminaUnitarias c cs =
  all (== eliminaUnitarias c cs)
      [f c cs | f <- [ eliminaUnitarias2
                     , eliminaUnitarias3
                     , eliminaUnitarias4
                     , eliminaUnitarias5
                     , eliminaUnitarias6
                     ]]

-- | Comprueba la equivalencia de las definiciones de
-- 'eliminaUnitarias'.
--
-- >>> verifica_equiv_eliminaUnitarias
-- +++ OK, passed 100 tests.
verifica_equiv_eliminaUnitarias :: IO ()
verifica_equiv_eliminaUnitarias =
  quickCheck prop_equiv_eliminaUnitarias

-- $doc
--
-- __Comparación de eficiencia__
--
-- > > length (eliminaUnitarias5 '3' (show (2^100000)))
-- > 27599
-- > (2.13 secs, 15,602,672 bytes)
-- > > length (eliminaUnitarias '3' (show (2^100000)))
-- > 27599
-- > (0.02 secs, 14,934,296 bytes)
-- 
-- > > length (eliminaUnitarias '3' (show (2^10000000)))
-- > 2766278
-- > (1.94 secs, 1,465,644,920 bytes)
-- > > length (eliminaUnitarias2 '3' (show (2^10000000)))
-- > 2766278
-- > (1.40 secs, 1,231,149,824 bytes)
-- > > length (eliminaUnitarias3 '3' (show (2^10000000)))
-- > 2766278
-- > (1.69 secs, 918,640,064 bytes)
-- > > length (eliminaUnitarias4 '3' (show (2^10000000)))
-- > 2766278
-- > (2.03 secs, 797,135,176 bytes)
-- > > length (eliminaUnitarias6 '3' (show (2^10000000)))
-- > 2766278
-- > (1.52 secs, 938,221,096 bytes)
