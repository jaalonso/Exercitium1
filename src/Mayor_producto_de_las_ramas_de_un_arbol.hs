-- Mayor_producto_de_las_ramas_de_un_arbol.hs
-- Mayor producto de las ramas de un árbol.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 18 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\ 
--    2   3           / | \
--        |          5  4  7
--        4          |     /\ 
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--    mayorProducto :: (Ord a, Num a) => Arbol a -> a
-- tal que (mayorProducto a) es el mayor producto de las ramas del árbol
-- a. Por ejemplo,
--    ghci> mayorProducto (N 1 [N 2 [], N  3 []])
--    3
--    ghci> mayorProducto (N 1 [N 8 [], N  4 [N 3 []]])
--    12
--    ghci> mayorProducto (N 1 [N 2 [],N 3 [N 4 []]])
--    12
--    ghci> mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    90
-- ---------------------------------------------------------------------

import Test.HUnit

data Arbol a = N a [Arbol a]
               deriving Show

-- 1º definición
mayorProducto1 :: (Ord a, Num a) => Arbol a -> a
mayorProducto1 (N x []) = x
mayorProducto1 (N x xs) = x * maximum [mayorProducto1 a | a <- xs]

-- Se puede usar map en lugar de comprensión:
mayorProducto1a :: (Ord a, Num a) => Arbol a -> a
mayorProducto1a (N x []) = x
mayorProducto1a (N x xs) = x * maximum (map mayorProducto1a xs)

-- 2ª definición
mayorProducto2 :: (Ord a, Num a) => Arbol a -> a
mayorProducto2 a = maximum [product xs | xs <- ramas a]

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ghci> ramas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- En la definición de mayorProducto2 se puede usar map en lugar de
-- comprensión. 
mayorProducto2a :: (Ord a, Num a) => Arbol a -> a
mayorProducto2a a = maximum (map product (ramas a))
