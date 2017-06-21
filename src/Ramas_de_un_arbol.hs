-- Ramas_de_un_arbol.hs
-- Ramas de un árbol.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 8 de Mayo de 2014
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
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]
-- 
-- Definir la función 
--    ramas :: Arbol b -> [[b]]
-- tal que (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ramas ej1  ==  [[1,2],[1,3,4]]
--    ramas ej2  ==  [[3,5,6],[3,4],[3,7,2],[3,7,1]]
-- ---------------------------------------------------------------------

import Test.HUnit

data Arbol a = N a [Arbol a]
               deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución:
ramas1 :: Arbol b -> [[b]]
ramas1 (N x []) = [[x]]
ramas1 (N x as) = [x : xs | a <- as, xs <- ramas1 a]

-- 2ª solución:
ramas2 :: Arbol b -> [[b]]
ramas2 (N x []) = [[x]]
ramas2 (N x as) = concat (map (map (x:)) (map ramas2 as))

-- 3ª solución:
ramas3 :: Arbol b -> [[b]]
ramas3 (N x []) = [[x]]
ramas3 (N x as) = concatMap (map (x:)) (map ramas3 as)

-- 4ª solución:
ramas4 :: Arbol a -> [[a]]
ramas4 (N x []) = [[x]]
ramas4 (N x xs) = map ramas4 xs >>= map (x:)

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Ángela
ramasA1 :: Arbol b -> [[b]]
ramasA1 (N x []) = [[x]]
ramasA1 (N x xs) = map (x:) (concat [ramasA1 ys | ys <- xs])

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

ramas :: Arbol a -> [[a]]
ramas = ramasA1

ejemplos :: Test
ejemplos =
    test ["1" ~: "ramas ej1" ~: 
          ramas ej1  ~?=  [[1,2],[1,3,4]],
          "2" ~: "ramas ej2" ~: 
          ramas ej2  ~?=  [[3,5,6],[3,4],[3,7,2],[3,7,1]]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
