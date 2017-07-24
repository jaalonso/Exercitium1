-- |
-- Module      : Lista_cuadrada
-- Description : Lista cuadrada.
-- Copyright   : J.A. Alonso (06-05-14)
-- License     : GPL-3
-- 
-- Definir la función
-- 
-- > listaCuadrada :: Int -> a -> [a] -> [[a]]
-- 
-- tal que __(listaCuadrada n x xs)__ es una lista de n listas de longitud n
-- formadas con los elementos de xs completada con x, si no xs no tiene
-- suficientes elementos. Por ejemplo,
-- 
-- >>> listaCuadrada 3 7 [0,3,5,2,4]
-- [[0,3,5],[2,4,7],[7,7,7]]
-- >>> listaCuadrada 3 7 [0..]
-- [[0,1,2],[3,4,5],[6,7,8]]
-- >>> listaCuadrada 2 'p' "eva"
-- ["ev","ap"]
-- >>> listaCuadrada 2 'p' ['a'..]
-- ["ab","cd"]
-- >>> listaCuadrada 1 0 ([]::[Int])
-- [[0]]

module Lista_cuadrada where

import Test.QuickCheck

-- | 1ª definición (con auxiliar).
listaCuadrada :: Int -> a -> [a] -> [[a]] 
listaCuadrada n x xs =
  take n (grupos n (xs ++ repeat x))

-- | (grupos n xs) es la lista obtenida agrupando los elementos de xs en
-- grupos de n elementos, salvo el último que puede tener menos. Por
-- ejemplo,
-- 
-- >>> grupos 2 [4,2,5,7,6]
-- [[4,2],[5,7],[6]]
-- >>> take 3 (grupos 3 [1..])
-- [[1,2,3],[4,5,6],[7,8,9]]
grupos :: Int -> [a] -> [[a]]
grupos _ [] = []
grupos n xs = take n xs : grupos n (drop n xs)

-- | 2ª definición (por comprensión).
listaCuadrada2 :: Int -> a -> [a] -> [[a]]
listaCuadrada2 n x xs = 
  take n [take n (drop m xs ++ repeat x)
         | m <- [0,n..n*n]]

-- | 3ª definición (por iteración).
listaCuadrada3 :: Int -> a -> [a] -> [[a]] 
listaCuadrada3 n x xs =
  take n [take n ys | ys <- iterate (drop n) (xs ++ repeat x)]

-- | 4ª definición (por iteración sin el último argumento).
listaCuadrada4 :: Int -> a -> [a] -> [[a]] 
listaCuadrada4 n x = 
  take n . map (take n) . iterate (drop n) . (++ repeat x)

-- | (prop_listaCuadrada n x xs) se verifica si las definiciones de
-- listaCuadrada son equivalentes sobre n, x, xs. Por ejemplo,
--
-- >>> prop_listaCuadrada (NonNegative 3) 7 [0,3,5,2,4]
-- True
-- >>> prop_listaCuadrada (NonNegative 3) 7 [0..]
-- True
prop_listaCuadrada :: NonNegative Int -> Int -> [Int] -> Bool
prop_listaCuadrada (NonNegative n) x xs =
  all (== listaCuadrada n x xs)
      [f n x xs | f <- [ listaCuadrada2
                       , listaCuadrada2
                       , listaCuadrada4
                       ]]

-- | Comprueba la equivalencia de las definiciones de listaCuadrada.
-- 
-- >>> verifica_listaCuadrada
-- +++ OK, passed 100 tests.
verifica_listaCuadrada :: IO ()
verifica_listaCuadrada =
  quickCheck prop_listaCuadrada
