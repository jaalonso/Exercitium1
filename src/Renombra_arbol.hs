-- Renombra_arbol.hs
-- Renombramiento de un árbol.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 5 de Septiembre de 2013
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Los árboles binarios se pueden representar mediante el
-- tipo Arbol definido por 
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, el árbol
--         "C"
--         / \ 
--        /   \
--       /     \
--     "B"     "A"
--     / \     / \
--   "A" "B" "B" "C" 
-- se puede definir por 
--    ej1 :: Arbol String
--    ej1 = N "C" (N "B" (H "A") (H "B")) (N "A" (H "B") (H "C"))
--
-- Definir la función
--    renombraArbol :: Arbol t -> Arbol Int
-- tal que (renombraArbol a) es el árbol obtenido sustituyendo el valor
-- de los nodos y hojas por números tales que tengan el mismo valor si y
-- sólo si coincide su contenido. Por ejemplo,
--    ghci> renombraArbol ej1
--    N 2 (N 1 (H 0) (H 1)) (N 0 (H 1) (H 2))
-- Gráficamente, 
--          2 
--         / \ 
--        /   \
--       /     \
--      1       0 
--     / \     / \
--    0   1   1   2  
-- Nótese que los elementos del árbol pueden ser de cualquier tipo. Por
-- ejemplo,
--    ghci> renombraArbol (N 9 (N 4 (H 8) (H 4)) (N 8 (H 4) (H 9)))
--    N 2 (N 0 (H 1) (H 0)) (N 1 (H 0) (H 2))
--    ghci> renombraArbol (N True (N False (H True) (H False)) (H True))
--    N 1 (N 0 (H 1) (H 0)) (H 1)
--    ghci> renombraArbol (N False (N False (H True) (H False)) (H True))
--    N 0 (N 0 (H 1) (H 0)) (H 1)
--    ghci> renombraArbol (H False)
--    H 0
--    ghci> renombraArbol (H True)
--    H 0
-- ---------------------------------------------------------------------

import Data.List (nub, sort)
import Data.Char (ord)       -- Para A2

data Arbol a = H a
             | N a (Arbol a) (Arbol a) 
             deriving (Show, Eq)

ej1 :: Arbol String
ej1 = N "C" (N "B" (H "A") (H "B")) (N "A" (H "B") (H "C"))

renombraArbol :: Ord t => Arbol t -> Arbol Int
renombraArbol a = aux a
    where ys            = valores a
          aux (H x)     = H (posicion x ys)
          aux (N x i d) = N (posicion x ys) (aux i) (aux d) 

-- (valores a) es la lista de los valores en los nodos y las hojas del
-- árbol a. Por ejemplo,
--    valores ej1  ==  ["A","B","C"]
valores :: Ord a => Arbol a -> [a]
valores a = sort (nub (aux a))
    where aux (H x)     = [x]
          aux (N x i d) = x : (aux i ++ aux d)

-- (posicion x ys) es la posición de x en ys. Por ejemplo.
--    posicion 7 [5,3,7,8]  ==  2
posicion :: Eq a => a -> [a] -> Int
posicion x ys = head [n | (y,n) <- zip ys [0..], y == x]
