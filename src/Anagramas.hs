-- Anagramas.hs
-- Anagramas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 29 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Una palabra es una anagrama de otra si se puede obtener
-- permutando sus letras. Por ejemplo, mora y roma son anagramas de
-- amor. 
-- 
-- Definir la función
--    anagramas :: String -> [String] -> [String]
-- tal que (anagramsFor x ys) es la lista de los elementos de ys que son
-- anagramas de x. Por ejemplo,
--    ghci> anagramas "amor" ["Roma","mola","loma","moRa", "rama"] 
--    ["Roma","moRa"]
--    ghci> anagramas "rama" ["aMar","amaRa","roMa","marr","aRma"]
--    ["aMar","aRma"]
-- ---------------------------------------------------------------------

import Data.List     (sort)
import Data.Char     (toLower)
import Data.Function (on)

-- 1ª definición (por recursión):
anagramas :: String -> [String] -> [String]
anagramas x [] = []
anagramas x (y:ys) | sonAnagramas x y = y : anagramas x ys
                   | otherwise        = anagramas x ys

-- (sonAnagramas xs ys) se verifica si xs e ys son anagramas. Por
-- ejemplo,
--    sonAnagramas "amor" "Roma"  ==  True
--    sonAnagramas "amor" "mola"  ==  False
sonAnagramas :: String -> String -> Bool
sonAnagramas xs ys = 
  sort (map toLower xs) == sort (map toLower ys) 

-- 2ª definición de sonAnagramas
sonAnagramas2 :: String -> String -> Bool
sonAnagramas2 xs ys = 
  (sort . map toLower) xs == (sort . map toLower) ys 

-- 3ª definición de sonAnagramas (con on)
sonAnagramas3 :: String -> String -> Bool
sonAnagramas3 = (==) `on` (sort . map toLower)

-- 2ª definición (por comprensión)
anagramas2 :: String -> [String] -> [String]
anagramas2 x ys = [y | y <- ys, sonAnagramas x y]

-- 3ª definición (con filter y sin el 2ª argumento)
anagramas3 :: String -> [String] -> [String]
anagramas3 x = filter (`sonAnagramas` x)

-- 4ª definición (sin sonAnagramas ni el 2º argumento)
anagramas4 :: String -> [String] -> [String]
anagramas4 x = filter (((==) `on` (sort . map toLower)) x)



