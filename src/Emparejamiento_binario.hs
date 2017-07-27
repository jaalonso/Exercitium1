-- |
-- Module      : Emparejamiento_binario
-- Description : Emparejamiento binario.
-- Copyright   : Exercitium (15-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
-- 
-- tal que __(zipBinario fs xs ys)__ es la lista obtenida aplicando cada una
-- de las operaciones binarias de fs a los correspondientes elementos de
-- xs e ys. Por ejemplo,
-- 
-- >>> zipBinario [(+), (*), (^)] [2,2,2] [4,4,4]
-- [6,8,16]
-- >>> zipBinario [(+)] [2,2,2] [4,4,4]
-- [6]
-- >>> zipBinario (cycle [(+), (*)]) [1 .. 4] [2..5]
-- [3,6,7,20]

module Emparejamiento_binario where

import Test.QuickCheck

-- | 1ª definición (por recursión).
zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario (f:fs) (x:xs) (y:ys) = f x y : zipBinario fs xs ys
zipBinario _ _ _                = []

-- | 2ª definición (con 'zip').
zipBinario2 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario2 fs xs ys = [f x y | (f,(x,y)) <- zip fs (zip xs ys)]

-- | 3ª definición (con 'zip3').
zipBinario3 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario3 fs xs ys = [f x y | (f,x,y) <- zip3 fs xs ys]

-- | 4ª definición (con 'zipWith3').
zipBinario4 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario4 = zipWith3 id 

-- | Tipo de operaciones para la verificación.
newtype Operacion = O (Int -> Int -> Int)

-- | Escritura de operaciones.
instance Show Operacion where
  show _ = ""

-- | Generador de operaciones.
gen_operaciones :: Gen [Operacion]
gen_operaciones =
  listOf (elements [O (+), O (-), O (*)])

-- | Comprueba la equivalencia de las definiciones de zipBinario.
--
-- >>> quickCheck prop_zipBinario
-- +++ OK, passed 100 tests.
prop_zipBinario :: [Int] -> [Int] -> Property
prop_zipBinario xs ys =
  forAll gen_operaciones
         (\os -> all (== zipBinario (fs os) xs ys)
                     [g (fs os) xs ys | g <- [ zipBinario2
                                              , zipBinario3
                                              , zipBinario4
                                              ]])
  where fs os = [f | (O f) <- os]
