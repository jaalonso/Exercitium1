-- Intercala_n_copias.hs
-- Intercalación de n copias.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 12 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función 
--    intercala :: Int -> a -> [a] -> [[a]]
-- tal que (intercala n x ys) es la lista de la listas obtenidas
-- intercalando n copias de x en ys. Por ejemplo,
--    intercala 2 'a' "bc" == ["bcaa","baca","baac","abca","abac","aabc"]
--    intercala 2 'a' "c"  == ["caa","aca","aac"]
--    intercala 1 'a' "c"  == ["ca","ac"]
--    intercala 0 'a' "c"  == ["c"]
-- Nota: No importa el orden de los elementos.
-- ---------------------------------------------------------------------

import Data.List 
import Test.QuickCheck


-- 1ª solución
-- ===========

intercala1 :: Int -> a -> [a] -> [[a]]
intercala1 0 _ xs     = [xs]
intercala1 n y []     = [replicate n y]
intercala1 n y (x:xs) = 
    concat [[replicate i y ++ (x:zs) | zs <- intercala1 (n-i) y xs]
            | i <- [0..n]]

-- 2ª solución
-- ===========

intercala2 :: Eq a => Int -> a -> [a] -> [[a]]
intercala2 n x ys = nub (aux n x ys)
    where 
      aux 0 _ ys = [ys]
      aux n x ys = concat [intercalaUno x zs | zs <- aux (n-1) x ys]

--    intercalaUno 'a' "bc"  == ["abc","bac","bca"]
intercalaUno :: a -> [a] -> [[a]]
intercalaUno x []     = [[x]]
intercalaUno x (y:ys) = (x:y:ys) : [y:zs | zs <- intercalaUno x ys]

-- Equivalencia
-- ============

prop_equivalencia :: Int -> Int -> [Int] -> Bool
prop_equivalencia n x ys =
    sort (nub (intercala1 m x ys)) == sort (intercala2 m x ys)
    where m = n `mod` 3
