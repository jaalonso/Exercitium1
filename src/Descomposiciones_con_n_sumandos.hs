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

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Luis
-- ====

sumasA1 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumasA1 n ys x = 
    nub (concat [permutations xs | xs <- combinacionesR n ys, sum xs == x])
    where combinacionesR _ []     = []
          combinacionesR 0 _      = [[]]
          combinacionesR k (x:xs) = 
              [x:ys | ys <- combinacionesR (k-1) (x:xs)] ++ combinacionesR k xs

-- Loles Valverde
-- ==============

sumasA2 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumasA2 n ys x = [xs | xs <- variacionesR n ys, sum xs == x]

variacionesR _ [] = [[]]
variacionesR 0 _  = [[]]
variacionesR k xs = [z:ys | z <- xs, ys <- variacionesR (k-1) xs]

-- Laura
-- =====

sumasA3 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumasA3 n ys x = [xs | xs <- comb n ys, sum xs == x, length xs == n]
    where comb n xs = nub (concatMap (permutations) (subsequences (concatMap (replicate n) xs)))

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    sumas 2 [1,2] 3    ==  [[1,2],[2,1]]             &&
    sumas 2 [1,2] 4    ==  [[2,2]]                   &&
    sumas 2 [1,2] 5    ==  []                        &&
    sumas 3 [1,2] 5    ==  [[1,2,2],[2,1,2],[2,2,1]] &&
    sumas 3 [1,2] 6    ==  [[2,2,2]]                 && 
    sumas 2 [1,2,5] 6  ==  [[1,5],[5,1]]
    where sumas = f



-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el ejercicio de 1HaskellADay propuesto el 11 de junio de
-- 2014 en http://bit.ly/1mH0DGT
