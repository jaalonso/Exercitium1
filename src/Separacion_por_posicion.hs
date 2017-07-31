-- |
-- Module      : Separacion_por_posicion
-- Description : Separación por posición.
-- Copyright   : Exercitium (09-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Separación por posición__
--
-- Definir la función
-- 
-- > particion :: [a] -> ([a],[a])
-- 
-- tal que __(particion xs)__ es el par cuya primera componente son los
-- elementos de xs en posiciones pares y su segunda componente son los
-- restantes elementos. Por ejemplo,
-- 
-- >>> particion [3,5,6,2]
-- ([3,6],[5,2])
-- >>> particion [3,5,6,2,7]
-- ([3,6,7],[5,2])
-- >>> particion "particion"
-- ("priin","atco")

module Separacion_por_posicion where

import Test.QuickCheck

-- | 1ª definición (por recursión y auxiliares con recursión cruzada).
particion :: [a] -> ([a],[a])
particion xs = (pares xs, impares xs)

-- | __(pares xs)__ es la lista de los elementos de xs en posiciones
-- pares. Por ejemplo,
-- 
-- >>> pares [3,5,6,2]
-- [3,6]
pares :: [a] -> [a]
pares []     = []
pares (x:xs) = x : impares xs

-- | __(impares xs)__ es la lista de los elementos de xs en posiciones
-- impares. Por ejemplo,
-- 
-- >>> impares [3,5,6,2]
-- [5,2]
impares :: [a] -> [a]
impares []     = []
impares (_:xs) = pares xs

-- | 2ª definición (por comprensión).
particion2 :: [a] -> ([a],[a])
particion2 xs =
  ([x | (x,y) <- ps, even y],
   [x | (x,y) <- ps, odd y])
  where ps = zip xs [0..]

-- | 3ª definición (por comprensión e índices)
particion3 :: [a] -> ([a],[a])
particion3 xs = 
  ([xs!!k | k <- [0,2..n]],
   [xs!!k | k <- [1,3..n]]) 
  where n = length xs - 1

-- | 4ª definición (por recursión).
particion4 :: [a] -> ([a],[a])
particion4 []     = ([],[])
particion4 (x:xs) = (x:zs,ys)
  where (ys,zs) = particion4 xs  

-- | 5ª definición (por plegado).
particion5 :: [a] -> ([a],[a])
particion5 =
  foldr f ([],[])
  where f x (ys,zs) = (x:zs,ys)

-- | __(prop_equiv_particion xs)__ se verifica si las definiciones de
-- 'particion' son equivalentes sobre xs. Por ejemplo,
--
-- >>> all prop_equiv_particion [[3,5,6,2], [3,5,6,2,7]]
-- True
prop_equiv_particion :: [Int] -> Bool
prop_equiv_particion xs =
  all (== particion xs)
      [f xs | f <- [ particion2
                   , particion3
                   , particion4
                   , particion5
                   ]]

-- | Comprueba la equivalencia de las definiciones de 'particion'.
-- 
-- >>> verifica_equiv_particion
-- +++ OK, passed 100 tests.
verifica_equiv_particion :: IO ()
verifica_equiv_particion =
  quickCheck prop_equiv_particion

-- $doc
--
-- __Comparación de eficiencia__
--
-- > > sum (snd (particion [1..20000]))
-- > 100010000
-- > (0.03 secs, 4,220,080 bytes)
-- > > sum (snd (particion2 [1..20000]))
-- > 100010000
-- > (0.05 secs, 9,817,496 bytes)
-- > > sum (snd (particion3 [1..20000]))
-- > 100010000
-- > (1.26 secs, 4,701,208 bytes)
-- > > sum (snd (particion4 [1..20000]))
-- > 100010000
-- > (0.06 secs, 9,979,608 bytes)
-- > > sum (snd (particion5 [1..20000]))
-- > 100010000
-- > (0.04 secs, 7,124,400 bytes)
-- > 
-- > > sum (snd (particion [1..10^6]))
-- > 250000500000
-- > (0.92 secs, 192,989,200 bytes)
-- > > sum (snd (particion2 [1..10^6]))
-- > 250000500000
-- > (2.24 secs, 472,989,112 bytes)
-- > > sum (snd (particion4 [1..10^6]))
-- > 250000500000
-- > (1.88 secs, 480,988,912 bytes)
-- > > sum (snd (particion5 [1..10^6]))
-- > 250000500000
-- > (2.12 secs, 339,451,600 bytes)
