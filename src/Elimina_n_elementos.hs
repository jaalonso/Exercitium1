-- Elimina_n_elementos.hs
-- Eliminación de n elementos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  9 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función
--    elimina :: Int -> [a] -> [[a]]
-- tal que (elimina n xs) es la lista de las listas obtenidas eliminando
-- n elementos de xs. Por ejemplo,
--    elimina 0 "abcd"  ==  ["abcd"]
--    elimina 1 "abcd"  ==  ["abc","abd","acd","bcd"]
--    elimina 2 "abcd"  ==  ["ab","ac","ad","bc","bd","cd"]
--    elimina 3 "abcd"  ==  ["a","b","c","d"]
--    elimina 4 "abcd"  ==  [""]
--    elimina 5 "abcd"  ==  []
--    elimina 6 "abcd"  ==  []
-- --------------------------------------------------------------------- 

import Data.List -- Para A1

elimina1 :: Int -> [a] -> [[a]]
elimina1 0 xs     = [xs]
elimina1 n []     = []
elimina1 n (x:xs) = [x:ys | ys <- elimina1 n xs] ++ elimina1 (n-1) xs

-- 2ª solución
elimina2 :: Int -> [a] -> [[a]]
elimina2 n xs = [ys | ys <- subsequences xs, length ys == k]
    where k = length xs - n
