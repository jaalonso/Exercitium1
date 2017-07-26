-- |
-- Module      : Primos_consecutivos_con_media_capicua
-- Description : Primos consecutivos con media capicúa.
-- Copyright   : Exercitium (28-04-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir las funciones
--
-- > primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
-- > nPrimosConsecutivosConMediaCapicua :: Int -> Int
-- 
-- tales que
--
-- * __primosConsecutivosConMediaCapicua__ es la lista de las ternas 
--   (x,y,z) tales que x e y son primos consecutivos cuya media, z, es
--   capicúa. Por ejemplo,
-- 
-- >>> take 5 primosConsecutivosConMediaCapicua
-- [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
--
-- * __(nPrimosConsecutivosConMediaCapicua n)__ es el número de ternas de
--   primos consecutivos con media capicua que son menores que n. Por
--   ejemplo, 
--
-- >>> nPrimosConsecutivosConMediaCapicua 2014
-- 20

module Primos_consecutivos_con_media_capicua where

import Data.Numbers.Primes (primes)

-- | 1ª definición (definiendo los primos)
primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua =
  [(x,y,z) | (x,y) <- zip primos (tail primos)
           , let z = (x + y) `div` 2
           , capicua z]

-- | (primo x) se verifica si x es primo. Por ejemplo,
-- >>> primo 7
-- True
-- >>> primo 8
-- False
primo :: Int -> Bool
primo x = [y | y <- [1..x]
             , x `rem` y == 0] == [1,x]

-- | primos es la lista de los números primos mayores que 2. Por
-- ejemplo,
-- 
-- >>> take 10 primos
-- [3,5,7,11,13,17,19,23,29,31]
primos :: [Int]
primos = [x | x <- [3,5..]
            , primo x]

-- | (capicua x) se verifica si x es capicúa. Por ejemplo,
--
-- >>> capicua 32723
-- True
-- >>> capicua 32732
-- False
capicua :: Int -> Bool
capicua x = ys == reverse ys
  where ys = show x

-- | 2ª definición (con primes)
primosConsecutivosConMediaCapicua2 :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua2 =
  [(x,y,z) | (x,y) <- zip primos3 (tail primos3)
           , let z = (x + y) `div` 2
           , capicua z]
  where primos3 = tail primes

-- | (verifica_primosConsecutivosConMediaCapicua n =) se verifica si las
-- definiciones de primosConsecutivosConMediaCapicua son equivalentes
-- para los n primeros elementos. Por ejemplo,
--
-- >>> verifica_primosConsecutivosConMediaCapicua 20
-- True
verifica_primosConsecutivosConMediaCapicua :: Int -> Bool
verifica_primosConsecutivosConMediaCapicua n =
  take n primosConsecutivosConMediaCapicua ==
  take n primosConsecutivosConMediaCapicua2

-- Comparación de eficiencia:
--    λ> primosConsecutivosConMediaCapicua !! 30
--    (12919,12923,12921)
--    (2.91 secs, 1,887,929,216 bytes)
--    λ> primosConsecutivosConMediaCapicua2 !! 30
--    (12919,12923,12921)
--    (0.01 secs, 0 bytes)

-- | Definición de nPrimosConsecutivosConMediaCapicua
nPrimosConsecutivosConMediaCapicua :: Int -> Int
nPrimosConsecutivosConMediaCapicua n = 
  length (takeWhile (\(_,y,_) -> y < n)
                    primosConsecutivosConMediaCapicua)
