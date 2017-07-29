-- |
-- Module      : Biparticiones_de_una_lista
-- Description : Biparticiones de una lista.
-- Copyright   : Exercitium (23-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > biparticiones :: [a] -> [([a],[a])]
-- 
-- tal que __(biparticiones xs)__ es la lista de pares formados por un
-- prefijo de xs y el resto de xs. Por ejemplo,
-- 
-- >>> biparticiones [3,2,5]
-- [([],[3,2,5]),([3],[2,5]),([3,2],[5]),([3,2,5],[])]
-- >>> biparticiones "Roma"
-- [("","Roma"),("R","oma"),("Ro","ma"),("Rom","a"),("Roma","")]

module Biparticiones_de_una_lista where

import Data.List           ( inits
                           , tails
                           )
import Control.Applicative ( liftA2
                           )
import Test.QuickCheck

-- | 1ª definición (con 'splitAt').
biparticiones :: [a] -> [([a],[a])]
biparticiones xs = [splitAt i xs | i <- [0..length xs]]

-- | 2ª definición (con 'inits' y 'tails').
biparticiones2 :: [a] -> [([a],[a])]
biparticiones2 xs = zip (inits xs) (tails xs)

-- | 3ª definición (por recursión).
biparticiones3 :: [a] -> [([a],[a])]
biparticiones3 [] = [([],[])] 
biparticiones3 (x:xs) = 
  ([],x:xs) : [(x:ys,zs) | (ys,zs) <- biparticiones3 xs]

-- | 4ª definición (con 'liftA2').
biparticiones4 :: [a] -> [([a],[a])]
biparticiones4 = liftA2 zip inits tails

-- | 5ª definición (con '<$>' y '<*>').
biparticiones5 :: [a] -> [([a],[a])]
biparticiones5 = zip <$> inits <*> tails

-- | __(prop_biparticiones xs)__ se verifica si las definiciones de
-- biparticiones son equivalentes sobre xs. Por ejemplo,
--
-- >>> prop_biparticiones [3,2,5]
-- True
prop_biparticiones :: [Int] -> Bool
prop_biparticiones xs =
  all (== biparticiones xs)
      [f xs | f <- [ biparticiones2
                   , biparticiones3
                   , biparticiones4
                   , biparticiones5
                   ]]

-- | Comprueba la equivalencia de las definiciones de biparticiones.
-- 
-- >>> verifica_biparticiones
-- +++ OK, passed 100 tests.
verifica_biparticiones :: IO ()
verifica_biparticiones =
  quickCheck prop_biparticiones 
