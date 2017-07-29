-- |
-- Module      : Enumera_arbol
-- Description : Enumeración de árboles binarios.
-- Copyright   : Exercitium (28-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
--
-- __Enumeración de árboles binarios__
--
-- Los árboles binarios se pueden representar mediante el tipo Arbol
-- definido por  
-- 
-- >    data Arbol a = H a 
-- >                 | N a (Arbol a) (Arbol a)
-- >                 deriving Show
-- 
-- Por ejemplo, el árbol
-- 
-- >         "B"
-- >         / \ 
-- >        /   \
-- >       /     \
-- >     "B"     "A"
-- >     / \     / \
-- >   "A" "B" "C" "C"
-- 
-- se puede definir por
-- 
-- >    ej1 :: Arbol String
-- >    ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))
--
-- Definir la función
-- 
-- > enumeraArbol :: Arbol t -> Arbol Int
-- 
-- tal que __(enumeraArbol a)__ es el árbol obtenido numerando las hojas y
-- los nodos de a desde la hoja izquierda hasta la raíz. Por ejemplo,
-- 
-- >>> enumeraArbol ej1
-- N 6 (N 2 (H 0) (H 1)) (N 5 (H 3) (H 4))
-- 
-- Gráficamente,
-- 
-- >          6 
-- >         / \ 
-- >        /   \
-- >       /     \
-- >      2       5 
-- >     / \     / \
-- >    0   1   3   4  

module Enumera_arbol where

-- | Tipo de dato de árboles binarios.
data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- | Ejemplo de árbol. 
ej1 :: Arbol String
ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))

-- | Definición.
enumeraArbol :: Arbol t -> Arbol Int
enumeraArbol a = fst (aux a 0)
  where aux :: Arbol a -> Int -> (Arbol Int,Int)
        aux (H _) n     = (H n, 1+n)
        aux (N _ i d) n = (N n2 i' d', 1+n2)
          where (i', n1) = aux i n
                (d', n2) = aux d n1
