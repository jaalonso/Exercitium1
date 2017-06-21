-- EmparejamientoDeArboles.hs
-- Emparejamiento de árboles.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 1 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\ 
--    6   3           / | \
--        |          5  4  7
--        5          |     /\ 
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 6 [],N 3 [N 5 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--    emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
-- tal que (emparejaArboles f a1 a2) es el árbol obtenido aplicando la
-- función f a los elementos de los árboles a1 y a2 que se encuentran en
-- la misma posición. Por ejemplo,
--    ghci> emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
--    N 2 [N 8 []]
--    ghci> emparejaArboles (+) ej1 ej2
--    N 4 [N 11 [],N 7 []]
--    ghci> emparejaArboles (+) ej1 ej1
--    N 2 [N 12 [],N 6 [N 10 []]]
-- ---------------------------------------------------------------------

import Test.HUnit

data Arbol a = N a [Arbol a]
               deriving (Show, Eq)

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

emparejaArboles1 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles1 f (N x l1) (N y l2) = 
    N (f x y) (zipWith (emparejaArboles1 f) l1 l2)

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Alberto Caro (incorrecta)
-- =========================

nodo (N a xs) = a

emparejaArbolesA1 f (N a []) (N b []) = (N (f a b) [])
emparejaArbolesA1 f (N a xs) (N b []) = (N (f a b) [])
emparejaArbolesA1 f (N a []) (N b xs) = (N (f a b) [])
emparejaArbolesA1 f (N a (x:xs)) (N b (y:ys)) = 
    (N (f a b) [emparejaArbolesA1 f (N (nodo x) xs) (N (nodo y) ys)])

-- Luis
-- ====
emparejaArbolesA2 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArbolesA2 f (N x xs) (N y ys) = 
    N (f x y) [emparejaArboles f a b | (a,b) <- zip xs ys]

-- David
-- =====

emparejaArbolesA4 :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArbolesA4 f (N a []) (N b []) = N (f a b) []
emparejaArbolesA4 f (N a xs) (N b ys) = N (f a b) zs
    where zs = zipWith (emparejaArbolesA4 f) xs ys

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles = emparejaArbolesA4

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
          ~?= N 2 [N 8 []],
          "2" ~: "ej2" ~:
          emparejaArboles (+) ej1 ej2
          ~?= N 4 [N 11 [],N 7 []],
          "3" ~: "ej3" ~:
          emparejaArboles (+) ej1 ej1
          ~?= N 2 [N 12 [],N 6 [N 10 []]]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}
