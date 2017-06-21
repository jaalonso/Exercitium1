-- n_gramas.hs
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
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Luis
-- ====

nGramasA1 :: Int -> [a] -> [[a]]
nGramasA1 k xs = combinaciones k xs
    where combinaciones 0 _ = [[]]
          combinaciones _ [] = []
          combinaciones k (x:xs) = [x:ys|ys<-combinaciones (k-1)xs] ++ combinaciones k xs

-- David
-- =====
nGramasA2 :: Int -> [a] -> [[a]]
nGramasA2 0 xs     = [take 0 xs]
nGramasA2 _ []     = []
nGramasA2 n (x:xs) = [x:y | y <- nGramasA2 (n-1) xs] ++ nGramasA2 n xs

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    nGramas 0 "abcd"  ==  [""]                            &&
    nGramas 1 "abcd"  ==  ["a","b","c","d"]               &&
    nGramas 2 "abcd"  ==  ["ab","ac","ad","bc","bd","cd"] &&
    nGramas 3 "abcd"  ==  ["abc","abd","acd","bcd"]       &&
    nGramas 4 "abcd"  ==  ["abcd"]                        &&
    nGramas 5 "abcd"  ==  []
    where nGramas = f

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en el ejercicio de 1HaskellADay del 3 de junio de 2014
-- publicado [aquí](http://bit.ly/1pdxx8l).
