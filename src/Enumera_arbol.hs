-- Enumera_arbol.hs
-- Enumeración de árboles binarios.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 5 de Septiembre de 2013
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los árboles binarios se pueden representar mediante el
-- tipo Arbol definido por 
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, el árbol
--         "B"
--         / \ 
--        /   \
--       /     \
--     "B"     "A"
--     / \     / \
--   "A" "B" "C" "C" 
-- se puede definir por 
--    ej1 :: Arbol String
--    ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))
--
-- Definir la función
--    enumeraArbol :: Arbol t -> Arbol Int
-- tal que (enumeraArbol a) es el árbol obtenido numerando las hojas y
-- los nodos de a desde la hoja izquierda hasta la raíz. Por ejemplo,
--    ghci> enumeraArbol ej1
--    N 6 (N 2 (H 0) (H 1)) (N 5 (H 3) (H 4))
-- Gráficamente, 
--          6 
--         / \ 
--        /   \
--       /     \
--      2       5 
--     / \     / \
--    0   1   3   4  
-- ---------------------------------------------------------------------

import Test.HUnit

data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

ej1 :: Arbol String
ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))

enumeraArbol1 :: Arbol t -> Arbol Int
enumeraArbol1 a = fst (aux a 0)
    where aux :: Arbol a -> Int -> (Arbol Int,Int)
          aux (H _) n     = (H n, n+1)
          aux (N x i d) n = (N n2 i' d', 1+n2)
                            where (i', n1) = aux i n
                                  (d', n2) = aux d n1

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Eduardo
-- =======

enumeraArbolA1 (N x ar1 ar2) = aux n (N x ar1 ar2)
    where 
      n                   = numerodeNodos (N x ar1 ar2) - 1
      aux y (H x)         = H y
      aux n (N x ar1 ar2) = N n (aux (n- numerodeNodos ar2 -1) ar1) (aux (n-1) ar2)

numerodeNodos (H x)         = 1
numerodeNodos (N x ar1 ar2) = 1 + numerodeNodos ar1 + numerodeNodos ar2

-- David
-- =====

enumeraArbolA2 :: Arbol t -> Arbol Int
enumeraArbolA2 a   = enumera [0..n] a
    where n = (numero a)-1
 
enumera :: [a] -> Arbol t -> Arbol a
enumera [x] (H a)     = H x
enumera xs  (N a b c) = N (last xs) (enumera ys  b) (enumera zs c )
    where (ys,zs) = splitAt n (init xs)
          n       = numero b
 
numero :: Num a => Arbol t -> a       
numero (H a)     = 1
numero (N a b c) = 1 + (numero b) + (numero c)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

enumeraArbol :: Arbol t -> Arbol Int
enumeraArbol = enumeraArbolA1

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          enumeraArbol (N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))) 
          ~?= N 6 (N 2 (H 0) (H 1)) (N 5 (H 3) (H 4)),
          "2" ~: "ej2" ~:
          enumeraArbol (N "B" (H "A") (N "A" (H "C") (H "C"))) 
          ~?= N 4 (H 0) (N 3 (H 1) (H 2)),
          "3" ~: "ej3" ~:
          enumeraArbol (N "B" (N "B" (H "A") (H "B")) (H "C")) 
          ~?= N 4 (N 2 (H 0) (H 1)) (H 3)]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}
