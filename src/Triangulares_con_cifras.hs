-- |
-- Module      : Triangulares_con_cifras
-- Description : Números triangulares con n cifras distintas.
-- Copyright   : Exercitium (27-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
--
-- __Números triangulares con n cifras distintas__
-- 
-- Los números triangulares se forman como sigue
-- 
-- >    *     *      * 
-- >         * *    * *
-- >               * * *
-- >    1     3      6
-- 
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
-- 
-- >     1 = 1
-- >     3 = 1+2
-- >     6 = 1+2+3
-- >    10 = 1+2+3+4
-- >    15 = 1+2+3+4+5
-- 
-- Definir la función
-- 
-- > triangularesConCifras :: Int -> [Integer]
-- 
-- tal que __(triangulares n)__ es la lista de los números triangulares con
-- n cifras distintas. Por ejemplo,
-- 
-- >>> take 6 (triangularesConCifras 1)
-- [1,3,6,55,66,666]
-- >>> take 6 (triangularesConCifras 2)
-- [10,15,21,28,36,45]
-- >>> take 6 (triangularesConCifras 3)
-- [105,120,136,153,190,210]
-- >>> take 5 (triangularesConCifras 4)
-- [1035,1275,1326,1378,1485]
-- >>> take 2 (triangularesConCifras 10)
-- [1062489753,1239845706]

module Triangulares_con_cifras where

import Data.List (nub)

-- | Definición. 
triangularesConCifras :: Int -> [Integer]
triangularesConCifras n =
    [x | x <- triangulares
       , nCifras x == n]

-- | __triangulares__ es la sucesión de los números triangulares. Por
-- ejemplo,
--
-- >>> take 15 triangulares1
-- [1,3,6,10,15,21,28,36,45,55,66,78,91,105,120]
triangulares1 :: [Integer]
triangulares1 = 1 : [x+y | (x,y) <- zip [2..] triangulares1]

-- | 2ª definición de 'triangulares' (usando 'zipWith').
triangulares2 :: [Integer]
triangulares2 = 1 : zipWith (+) [2..] triangulares1

-- | 3ª definición de 'triangulares' (usando 'scanl').
triangulares3 :: [Integer]
triangulares3 = scanl (+) 1 [2..]

-- | 4ª definición de triangulares (con la fórmula)
triangulares4 :: [Integer]
triangulares4 = [(n*(n+1)) `div` 2 | n <- [1..]]

-- | __(prop_triangulares n)__ se verifica si las definiciones de
-- triangulares son equivalentes en sus primeros n términos. Por
-- ejemplo,
--
-- >>> prop_triangulares 100
-- True
prop_triangulares :: Int -> Bool
prop_triangulares n =
  all (== take n triangulares1)
      [take n f | f <- [ triangulares2
                       , triangulares3
                       , triangulares4
                       ]]

-- $nota
--
-- Comparación de eficiencia:
-- 
-- > > sum (take (10^6) triangulares1)
-- > 166667166667000000
-- > (1.07 secs, 475,647,576 bytes)
-- > > sum (take (10^6) triangulares2)
-- > 166667166667000000
-- > (1.23 secs, 667,646,128 bytes)
-- > > sum (take (10^6) triangulares3)
-- > 166667166667000000
-- > (0.50 secs, 370,832,960 bytes)
-- > > sum (take (10^6) triangulares4)
-- > 166667166667000000
-- > (1.09 secs, 489,997,432 bytes)

-- | Usaremos como __triangulares__ la 3ª definición.
triangulares :: [Integer]
triangulares = triangulares3

-- | __(nCifras x)__ es el número de cifras distintas del número x. Por
-- ejemplo,
-- 
-- >>> nCifras 325275
-- 4
nCifras :: Integer -> Int
nCifras = length . nub . show
