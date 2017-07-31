-- |
-- Module      : Ordenada_ciclicamente
-- Description : Ordenada cíclicamente.
-- Copyright   : Exercitium (12-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Ordenada cíclicamente__
-- 
-- Se dice que una sucesión x(1), ..., x(n) está ordenada cíclicamente
-- si existe un índice i tal que la sucesión  
--    x(i), x(i+1), ..., x(n), x(1), ..., x(i-1)
-- está ordenada crecientemente. 
-- 
-- Definir la función
-- 
-- > ordenadaCiclicamente :: Ord a => [a] -> Int
-- 
-- tal que __(ordenadaCiclicamente xs)__ es el índice (empezando en 1) a
-- partir del cual está ordenada, si el la lista está ordenado cíclicamente
-- y 0 en caso contrario. Por ejemplo,
-- 
-- >>> ordenadaCiclicamente [1,2,3,4]
-- 1  
-- >>> ordenadaCiclicamente [5,8,2,3]
-- 3 
-- >>> ordenadaCiclicamente [4,6,7,5,4,3]
-- 0 
-- >>> ordenadaCiclicamente [1,0,1,2]
-- 0
-- >>> ordenadaCiclicamente [0,2,0]
-- 3
-- >>> ordenadaCiclicamente "cdeab"
-- 4

module Ordenada_ciclicamente where

import Data.List (sort)
import Test.QuickCheck

-- | 1ª definición (por comprensión).
ordenadaCiclicamente :: Ord a => [a] -> Int
ordenadaCiclicamente xs = 
  primero [n+1 | n <- [0..length xs-1]
               , ordenada (drop n xs ++ take n xs)]
  where primero []    = 0
        primero (x:_) = x

-- | __(ordenada xs)__ se verifica si la lista xs está ordenada
-- crecientemente. Por ejemplo,
-- 
-- >>> ordenada "acd"
-- True
-- >>> ordenada "acdb"
-- False
ordenada :: Ord a => [a] -> Bool 
ordenada (x:y:zs) = x <= y && ordenada (y:zs) 
ordenada _        = True 
 
-- | 2ª definición (por recursión)
ordenadaCiclicamente2 :: Ord a => [a] -> Int
ordenadaCiclicamente2 xs = aux xs 1 (length xs)  
  where aux ys i k 
          | i > k       = 0 
          | ordenada ys = i 
          | otherwise   = aux (siguienteCiclo ys) (i+1) k 

-- | __(siguienteCiclo xs)__ es la lista obtenida añadiendo el primer elemento
-- de xs al final del resto de xs. Por ejemplo,
-- 
-- >>> siguienteCiclo [3,2,5]
-- [2,5,3]
siguienteCiclo :: [a] -> [a]
siguienteCiclo []     = [] 
siguienteCiclo (x:xs) = xs ++ [x] 

-- | Generador de listas ordenadas cíclicamente.
--
-- > > sample listaOrdenadaCicliamente
-- > []
-- > [0,0]
-- > [-4,4]
-- > [2,4,-3]
-- > [2,6,8,-7,-1,1]
-- > [3,-1]
-- > [-11,-9,-5,-4,0,0,2]
-- > [-10,8]
-- > [-9,-7,-1,5,-13,-9]
-- > [-7,3,17,-15]
-- > []
listaOrdenadaCicliamente :: Gen [Int]
listaOrdenadaCicliamente = do
  xs <- arbitrary
  n  <- choose (0,length xs - 1)
  let ys = sort xs
  return (drop n ys ++ take n ys)

-- | Comprobación de la equivalencia de las definiciones de
-- 'ordenadaCiclicamente' sobre listas ordenadas cíclicamente.
--
-- >>> quickCheck prop_equiv_ordenadaCiclicamente1
-- +++ OK, passed 100 tests.
prop_equiv_ordenadaCiclicamente1 :: Property
prop_equiv_ordenadaCiclicamente1 =
  forAll listaOrdenadaCicliamente
         (\xs -> ordenadaCiclicamente xs == ordenadaCiclicamente2 xs)

-- | Comprobación de la equivalencia de las definiciones de
-- 'ordenadaCiclicamente'.
--
-- >>> quickCheck prop_equiv_ordenadaCiclicamente2
-- +++ OK, passed 100 tests.
prop_equiv_ordenadaCiclicamente2 :: [Int] -> Bool
prop_equiv_ordenadaCiclicamente2 xs =
  ordenadaCiclicamente xs == ordenadaCiclicamente2 xs
