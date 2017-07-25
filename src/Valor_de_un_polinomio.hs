-- |
-- Module      : Valor_de_un_polinomio
-- Description : Valores de polinomios representados con vectores.
-- Copyright   : J.A. Alonso (07-05-14)
-- License     : GPL-3
-- 
-- Los polinomios se pueden representar mediante vectores usando la
-- librería Data.Array. En primer lugar, se define el tipo de los
-- polinomios (con coeficientes de tipo a) mediante
--
-- > type Polinomio a = Array Int a
-- 
-- Como ejemplos, definimos el polinomio
-- 
-- > ej_pol1 :: Array Int Int
-- > ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]
--
-- que representa a 2x - 5x^2 + 7x^4 + 6 y el polinomio
-- 
-- > ej_pol2 :: Array Int Double
-- > ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]
-- 
-- que representa a 2x - 5.2x^2 + 7x^4 + 6.5
--
-- Definir la función
-- 
-- > valor :: Num a => Polinomio a -> a -> a
-- 
-- tal que (valor p b) es el valor del polinomio p en el punto b. Por
-- ejemplo,
-- 
-- >>> valor ej_pol1 0
-- 6
-- >>> valor ej_pol1 1
-- 10
-- >>> valor ej_pol1 2
-- 102
-- >>> valor ej_pol2 0
-- 6.5
-- >>> valor ej_pol2 1
-- 10.3
-- >>> valor ej_pol2 3
-- 532.7

module Valor_de_un_polinomio where

import Data.Array
import Test.QuickCheck

-- | Tipo de polinomios
type Polinomio a = Array Int a

-- | Ejemplo de polinomio: 2x - 5x^2 + 7x^4 + 6
ej_pol1 :: Array Int Int
ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]

-- | Ejemplo de polinomio: 2x - 5.2x^2 + 7x^4 + 6.5
ej_pol2 :: Array Int Double
ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]

-- | 1ª definición.
valor :: Num a => Polinomio a -> a -> a
valor p b = sum [(p!i)*b^i | i <- [0..n]]
  where (_,n) = bounds p

-- | 2ª definición.
valor2 :: Num a => Polinomio a -> a -> a
valor2 p b = sum [v*b^i | (i,v) <- assocs p]

-- | Comprueba la equivalencia de las definiciones de valor.
--
-- >>> equivalencia_valor
-- +++ OK, passed 100 tests.
equivalencia_valor :: IO ()
equivalencia_valor =
  quickCheck prop_valor
  where prop_valor :: [Int] -> Int -> Bool
        prop_valor xs b =
          valor p b == valor2 p b
          where p = listArray (0,length xs - 1) xs

