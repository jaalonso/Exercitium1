-- |
-- Module      : Anagramas
-- Description : Anagramas.
-- Copyright   : José A. Alonso (29-04-17)
-- License     : GPL-3
-- 
-- Una palabra es una anagrama de otra si se puede obtener permutando
-- sus letras. Por ejemplo, "mora" y "roma" son anagramas de "amor". 
-- 
-- Definir la función
-- 
-- > anagramas :: String -> [String] -> [String]
--
-- tal que (anagramas x ys) es la lista de los elementos de ys que son
-- anagramas de x. Por ejemplo,
-- 
-- >>> anagramas "amor" ["Roma","mola","loma","moRa", "rama"] 
-- ["Roma","moRa"]
-- >>> anagramas "rama" ["aMar","amaRa","roMa","marr","aRma"]
-- ["aMar","aRma"]

module Anagramas
  ( anagramas
  , anagramas2
  , anagramas3
  , anagramas4
  , sonAnagramas
  , sonAnagramas2
  , sonAnagramas3
  ) where

import Data.List     (sort)
import Data.Char     (toLower)
import Data.Function (on)

-- | 1ª definición (por recursión):
anagramas :: String -> [String] -> [String]
anagramas _ [] = []
anagramas x (y:ys) | sonAnagramas x y = y : anagramas x ys
                   | otherwise        = anagramas x ys

-- | (sonAnagramas xs ys) se verifica si xs e ys son anagramas. Por
-- ejemplo,
-- 
-- >>> sonAnagramas "amor" "Roma"
-- True
-- >>> sonAnagramas "amor" "mola"
-- False
sonAnagramas :: String -> String -> Bool
sonAnagramas xs ys = 
  sort (map toLower xs) == sort (map toLower ys) 

-- | 2ª definición de sonAnagramas
-- 
-- >>> sonAnagramas2 "amor" "Roma"
-- True
-- >>> sonAnagramas2 "amor" "mola"
-- False
sonAnagramas2 :: String -> String -> Bool
sonAnagramas2 xs ys = 
  (sort . map toLower) xs == (sort . map toLower) ys 

-- | 3ª definición de sonAnagramas (con on)
-- 
-- >>> sonAnagramas3 "amor" "Roma"
-- True
-- >>> sonAnagramas3 "amor" "mola"
-- False
sonAnagramas3 :: String -> String -> Bool
sonAnagramas3 = (==) `on` (sort . map toLower)

-- | 2ª definición (por comprensión)
-- 
-- >>> anagramas2 "amor" ["Roma","mola","loma","moRa", "rama"] 
-- ["Roma","moRa"]
-- >>> anagramas2 "rama" ["aMar","amaRa","roMa","marr","aRma"]
-- ["aMar","aRma"]
anagramas2 :: String -> [String] -> [String]
anagramas2 x ys = [y | y <- ys, sonAnagramas x y]

-- | 3ª definición (con filter y sin el 2ª argumento)
-- 
-- >>> anagramas3 "amor" ["Roma","mola","loma","moRa", "rama"] 
-- ["Roma","moRa"]
-- >>> anagramas3 "rama" ["aMar","amaRa","roMa","marr","aRma"]
-- ["aMar","aRma"]
anagramas3 :: String -> [String] -> [String]
anagramas3 x = filter (`sonAnagramas` x)

-- | 4ª definición (sin sonAnagramas ni el 2º argumento)
-- 
-- >>> anagramas4 "amor" ["Roma","mola","loma","moRa", "rama"] 
-- ["Roma","moRa"]
-- >>> anagramas4 "rama" ["aMar","amaRa","roMa","marr","aRma"]
-- ["aMar","aRma"]
anagramas4 :: String -> [String] -> [String]
anagramas4 x = filter (((==) `on` (sort . map toLower)) x)

-- Comprobación:
--    > stack exec doctest src/Anagramas.hs 
--    Examples: 14  Tried: 14  Errors: 0  Failures: 0

