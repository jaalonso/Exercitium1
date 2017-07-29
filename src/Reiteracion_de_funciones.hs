-- |
-- Module      : Reiteracion_de_funciones
-- Description : Reiteración de una función.
-- Copyright   : Exercitium (30-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
--
-- __Reiteración de una función__
-- 
-- Definir la función
-- 
-- > reiteracion :: Int -> (a -> a) -> a -> a
-- 
-- tal que __(reiteracion n f x)__ es el resultado de aplicar n veces la
-- función f a x. Por ejemplo,
-- 
-- >>> reiteracion 10 (+1) 5
-- 15
-- >>> reiteracion 10 (+5) 0
-- 50
-- >>> reiteracion  4 (*2) 1
-- 16
-- >>> reiteracion 4 (5:) []
-- [5,5,5,5]
-- 
-- Comprobar con QuickCheck que se verifican las siguientes propiedades
-- 
-- > reiteracion 10 (+1) x  == 10 + x 
-- > reiteracion 10 (+x) 0  == 10 * x 
-- > reiteracion 10 (x:) [] == replicate 10 x  

module Reiteracion_de_funciones where

import Test.QuickCheck
import Test.QuickCheck.Function

-- | 1ª definición (por recursión).
reiteracion :: Int -> (a -> a) -> a -> a
reiteracion 0 _ x = x
reiteracion n f x = f (reiteracion (n-1) f x)

-- | 2ª definición (por recursión sin el 3ª argumento).
reiteracion2 :: Int -> (a -> a) -> a -> a
reiteracion2 0 _ = id
reiteracion2 n f = f . reiteracion2 (n-1) f

-- | 3ª definición (con 'iterate').
reiteracion3 :: Int -> (a -> a) -> a -> a
reiteracion3 n f x = iterate f x !! n

-- | Comprobación de la equivalencia de las definiciones de
--'reiteracion'. 
--
-- >>> quickCheck prop_equiv_reiteracion
-- +++ OK, passed 100 tests.
prop_equiv_reiteracion :: (Positive Int) -> (Fun Int Int) -> Int -> Bool
prop_equiv_reiteracion (Positive n) (Fn f) x =
  all (== reiteracion n f x)
      [g n f x | g <- [ reiteracion2
                      , reiteracion3
                      ]]
prop_equiv_reiteracion (Positive _) _ _ =
  error "Por completitud"

-- | Comprobación de propiedades.
-- 
-- >>> quickCheck prop_reiteracion
-- +++ OK, passed 100 tests.
prop_reiteracion :: Int -> Bool
prop_reiteracion x =
     reiteracion 10 (+1) x  == 10 + x 
  && reiteracion 10 (+x) 0  == 10 * x 
  && reiteracion 10 (x:) [] == replicate 10 x  
  
