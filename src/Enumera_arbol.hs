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
