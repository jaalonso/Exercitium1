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

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Agustín
mayorProductoA1 :: (Ord a, Num a) => Arbol a -> a
mayorProductoA1 (N x as) = maximum [product y | y <- ramasA1 (N x as)]

ramasA1 :: Arbol b -> [[b]]
ramasA1 (N x []) = [[x]]
ramasA1 (N x as) = [x : xs | a <- as, xs <- ramasA1 a]

-- Luis
-- data Arbol a = N a [Arbol a]
--                deriving Show

mayorProductoA2 :: (Ord a, Num a) => Arbol a -> a
mayorProductoA2 a = maximum [product xs | xs <- ramas a]
    where ramas (N x []) = [[x]]
          ramas (N x xs) = map ([x]++) (concat [ramas ys | ys <- xs])

-- David
mayorProductoA3 :: (Ord a, Num a) => Arbol a -> a
mayorProductoA3 (N a []) = a
mayorProductoA3 (N a xs) = maximum [a*mayorProductoA3 y | y <- xs]

-- Ángela
mayorProductoA4 :: (Ord a, Num a) => Arbol a -> a
mayorProductoA4 (N x []) = x
mayorProductoA4 (N x xs) = x * maximum [mayorProducto ys | ys <- xs]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

mayorProducto :: (Ord a, Num a) => Arbol a -> a
mayorProducto = mayorProductoA3

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~: 
          mayorProducto (N 1 [N 2 [], N  3 []]) ~?= 3,
          "2" ~: "ej2" ~: 
          mayorProducto (N 1 [N 8 [], N  4 [N 3 []]]) ~?= 12,
          "3" ~: "ej3" ~: 
          mayorProducto (N 1 [N 2 [],N 3 [N 4 []]]) ~?= 12,
          "4" ~: "ej4" ~: 
          mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]) ~?= 90]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
