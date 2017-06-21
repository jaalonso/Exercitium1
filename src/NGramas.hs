-- NGramas.hs
-- N gramas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 9 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un n-grama de una sucesión es una subsucesión de n elementos.
--
-- Definir la función
--    nGramas :: Int -> [a] -> [[a]]
-- tal que (nGramas k xs) es la lista de los n-gramas de xs de longitud
-- k. Por ejemplo,
--    nGramas 0 "abcd"  ==  [""]
--    nGramas 1 "abcd"  ==  ["a","b","c","d"]
--    nGramas 2 "abcd"  ==  ["ab","ac","ad","bc","bd","cd"]
--    nGramas 3 "abcd"  ==  ["abc","abd","acd","bcd"]
--    nGramas 4 "abcd"  ==  ["abcd"]
--    nGramas 5 "abcd"  ==  []
-- ---------------------------------------------------------------------

nGramas1 :: Int -> [a] -> [[a]]
nGramas1 0 xs     = [[]]
nGramas1 n []     = []
nGramas1 n (x:xs) = [x:ys | ys <- nGramas1 (n-1) xs] ++ nGramas1 n xs

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en el ejercicio de 1HaskellADay del 3 de junio de 2014
-- publicado [aquí](http://bit.ly/1pdxx8l).
