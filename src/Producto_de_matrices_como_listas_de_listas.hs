-- Producto_de_matrices_como_listas_de_listas.hs
-- Producto de matrices como listas de listas
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 12 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices pueden representarse mediante una lista de listas donde
-- cada una de las lista representa una fila  de la matriz. Por ejemplo,
-- la matriz  
--    |1 0 -2|
--    |0 3 -1|
-- puede representarse por [[1,0,-2],[0,3,-1]]. 
-- 
-- Definir la función
--    producto :: Num a => [[a]] -> [[a]] -> [[a]]
-- tal que (producto p q) es el producto de las matrices p y q. Por
-- ejemplo, 
--    ghci> producto [[1,0,-2],[0,3,-1]] [[0,3],[-2,-1],[0,4]]
--    [[0,-5],[-6,-7]]
-- ---------------------------------------------------------------------

import Data.List (transpose)
import Data.Matrix hiding (transpose)           -- Para A2

producto :: Num a => [[a]] -> [[a]] -> [[a]]
producto p q = 
    [[sum [x*y | (x,y) <- zip fil col] | col <- transpose q] | fil <- p]

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Eduardo
-- =======

productoA1 p q = 
    separa (numeroFilas p) [sum (zipWith (*) (fila i p) (columna j q)) |
                            i <- [1..numeroFilas p], j <- [1..numeroFilas p]]

separa n [] = []
separa n xs = [take n xs] ++ separa n (drop n xs)

columna j xss = map (!!(j-1)) xss

fila i xss = xss !! (i-1)

numeroFilas xss = length xss

numeroColumnas xss = length (head xss)

-- David
-- =====

-- import Data.Matrix

productoA2 :: Num a => [[a]] -> [[a]] -> [[a]]
productoA2 p q = [filas i | i <- [1..m]]
 where p'      = fromLists p
       q'      = fromLists q
       ma      = multStd p' q'
       (m,n)   = (nrows ma,ncols ma)
       filas i = [ma!(i,j) | j <- [1..n]]

-- productoA3 :: Num a => [[a]] -> [[a]] -> [[a]]
productoA3 p q = fromLists p * fromLists q
