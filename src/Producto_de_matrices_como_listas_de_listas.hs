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
