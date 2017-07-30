-- |
-- Module      : Alergias
-- Description : Código de las alergias.
-- Copyright   : Exercitium (03-06-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Código de las alergias__
-- 
-- Para la determinación de las alergia se utiliza los siguientes
-- códigos para los alérgenos:
--
-- >   Huevos ........   1
-- >   Cacahuetes ....   2
-- >   Mariscos ......   4
-- >   Fresas ........   8
-- >   Tomates .......  16
-- >   Chocolate .....  32
-- >   Polen .........  64
-- >   Gatos ......... 128
-- 
-- Así, si Juan es alérgico a los cacahuetes y al chocolate, su
-- puntuación es 34 (es decir, 2+32).
-- 
-- Los alérgenos se representan mediante el siguiente tipo de dato
-- 
-- >  data Alergeno = Huevos
-- >                | Cacahuetes
-- >                | Mariscos
-- >                | Fresas
-- >                | Tomates
-- >                | Chocolate
-- >                | Polen
-- >                | Gatos
-- >                deriving (Enum, Eq, Show, Bounded)
-- 
-- Definir la función
-- 
-- > alergias :: Int -> [Alergeno]
-- 
-- tal que __(alergias n)__ es la lista de alergias correspondiente a una
-- puntuación n. Por ejemplo,
-- 
-- >>> alergias 0
-- []
-- >>> alergias 1
-- [Huevos]
-- >>> alergias 2
-- [Cacahuetes]
-- >>> alergias 8
-- [Fresas]
-- >>> alergias 3
-- [Huevos,Cacahuetes]
-- >>> alergias 5
-- [Huevos,Mariscos]
-- >>> alergias 248
-- [Fresas,Tomates,Chocolate,Polen,Gatos]
-- >>> alergias 255
-- [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
-- >>> alergias 509
-- [Huevos,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]

module Alergias where

import Data.List (subsequences)
import Test.QuickCheck

-- | Tipo de alergenos.
data Alergeno = Huevos
              | Cacahuetes
              | Mariscos
              | Fresas
              | Tomates
              | Chocolate
              | Polen
              | Gatos
              deriving (Enum, Eq, Show, Bounded)

-- | 1ª definición (usando números binarios).
alergias :: Int -> [Alergeno]
alergias n = 
  [toEnum x | (y,x) <- zip (int2bin n) [0..7]
            , y == 1]

-- | __(int2bin n)__ es la representación binaria del número n. Por
-- ejemplo,
-- 
-- >>> int2bin 10
-- [0,1,0,1]
--
-- ya que 10 = 0*1 + 1*2 + 0*4 + 1*8
int2bin :: Int -> [Int]
int2bin n | n < 2     = [n]
          | otherwise = n `rem` 2 : int2bin (n `div` 2)

-- | 2ª definición (sin usar números binarios).
alergias2 :: Int -> [Alergeno]
alergias2 n = map toEnum (aux n 0)
  where aux 0 _                = []
        aux _ a | a > 7        = []
        aux 1 a                = [a]
        aux m a | rem m 2 == 0 = aux (div m 2) (1+a)
                | otherwise    = a : aux (div m 2) (1+a) 

-- | 3ª definición (con combinaciones).
alergias3 :: Int -> [Alergeno]
alergias3 n = 
  [a | (a,c) <- zip alergenos codigos
     , c `elem` descomposicion n]

-- | __alergenos__ es la lista de los alergenos
alergenos :: [Alergeno]
alergenos = [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]

-- | __codigos__ es la lista de los códigos de los alergenos.
codigos :: [Int]
codigos = [2^x| x <- [0..7]]
    
-- | __(descomposicion n)__ es la descomposición de n (módulo 256) como sumas
-- de potencias de 2. Por ejemplo,
-- 
-- >>> descomposicion 3
-- [1,2]
-- >>> descomposicion 5
-- [1,4]
-- >>> descomposicion 248
-- [8,16,32,64,128]
-- >>> descomposicion 255
-- [1,2,4,8,16,32,64,128]
-- >>> descomposicion 509
-- [1,4,8,16,32,64,128]
descomposicion :: Int -> [Int]                     
descomposicion n = 
  head [xs | xs <- subsequences codigos
           , sum xs == n `mod` 256]

-- | Comprobación de la equivalencia de las definiciones de 'alergia'.
--
-- >>> quickCheck prop_equiv_alergias
-- +++ OK, passed 100 tests.
prop_equiv_alergias :: (NonNegative Int) -> Bool
prop_equiv_alergias (NonNegative n) =
  all (== alergias n )
      [f n | f <- [ alergias2
                  , alergias3
                  ]]
