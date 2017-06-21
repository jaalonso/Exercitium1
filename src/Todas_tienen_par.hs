-- Todas_tienen_par.hs
-- Todas tienen par.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 12 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir el predicado
--    todasTienenPar :: [[Int]] -> Bool
-- tal que tal que (todasTienenPar xss) se verifica si cada elemento de
-- la lista de listas xss contiene algún número par. Por ejemplo, 
--    todasTienenPar [[1,2],[3,4,5],[8]]  ==  True
--    todasTienenPar [[1,2],[3,5]]        ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
-- ===============================

todasTienenPar :: [[Int]] -> Bool
todasTienenPar xss = and [or [even x | x <- xs] | xs <- xss]

-- 2ª definición (por recursión)
-- =============================

todasTienenPar2 :: [[Int]] -> Bool
todasTienenPar2 []       = True
todasTienenPar2 (xs:xss) = tienePar xs && todasTienenPar2 xss

-- (tienePar xs) se verifica si xs contiene algún número par. 
tienePar  :: [Int] -> Bool
tienePar []     = False
tienePar (x:xs) = even x || tienePar xs

-- 3ª definición (por plegado)
-- ===========================

todasTienenPar3 :: [[Int]] -> Bool
todasTienenPar3 = foldr ((&&) . tienePar3) True 

-- (tienePar3 xs) se verifica si xs contiene algún número par. 
tienePar3  :: [Int] -> Bool
tienePar3 = foldr ((||) . even) False 

-- 4ª definición (con cuantificadores)
-- ===================================

todasTienenPar4 :: [[Int]] -> Bool
todasTienenPar4 = all (any even)

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- David
-- =====
todasTienenParA1 :: [[Int]] -> Bool
todasTienenParA1 xss = and $ map tienenPar xss
    where tienenPar xs = filter (even) xs /= []

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    todasTienenPar [[1,2],[3,4,5],[8]]  ==  True &&
    todasTienenPar [[1,2],[3,5]]        ==  False
    where todasTienenPar = f
