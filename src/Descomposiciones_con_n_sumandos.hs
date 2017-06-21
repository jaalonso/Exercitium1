-- Descomposiciones_con_n_sumandos.hs
-- Descomposiciones de x como sumas de n sumandos en una lista.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumas :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
-- tal que (sumas n ys x) es la lista de las descomposiciones de x como
-- sumas de n sumandos en la lista ns. Por ejemplo,
--    sumas 2 [1,2] 3      ==  [[1,2],[2,1]]
--    sumas 2 [1,2] 4      ==  [[2,2]]
--    sumas 2 [1,2] 5      ==  []
--    sumas 3 [1,2] 5      ==  [[1,2,2],[2,1,2],[2,2,1]]
--    sumas 3 [1,2] 6      ==  [[2,2,2]]
--    sumas 2 [1,2,5] 6    ==  [[1,5],[5,1]]
--    sumas 3 [1..1000] 4  ==  [[1,1,2],[1,2,1],[2,1,1]]
-- ---------------------------------------------------------------------

import Data.List    -- para A1

sumas1 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas1 1 ys x | x `elem` ys = [[x]]
              | otherwise   = []
sumas1 n ys x = 
    concat [[y:zs | zs <- sumas1 (n-1) ys (x-y)] | y <- ys, y <= x]
