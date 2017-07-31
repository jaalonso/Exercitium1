-- |
-- Module      : EmparejamientoDeArboles
-- Description : Emparejamiento de árboles.
-- Copyright   : Exercitium (10-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Emparejamiento de árboles__
-- 
-- Los árboles se pueden representar mediante el siguiente tipo de datos
-- 
-- > data Arbol a = N a [Arbol a]
-- >   deriving Show
-- 
-- Por ejemplo, los árboles
--
-- >   1               3
-- >  / \             /|\ 
-- > 6   3           / | \
-- >     |          5  4  7
-- >     5          |     /\ 
-- >                6    2  1
-- 
-- se representan por
-- 
-- > ej1, ej2 :: Arbol Int
-- > ej1 = N 1 [N 6 [],N 3 [N 5 []]]
-- > ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
-- 
-- > emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
-- 
-- tal que __(emparejaArboles f a1 a2)__ es el árbol obtenido aplicando la
-- función f a los elementos de los árboles a1 y a2 que se encuentran en
-- la misma posición. Por ejemplo,
-- 
-- >>> emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
-- N 2 [N 8 []]
-- >>> emparejaArboles (+) ej1 ej2
-- N 4 [N 11 [],N 7 []]
-- >>> emparejaArboles (+) ej1 ej1
-- N 2 [N 12 [],N 6 [N 10 []]]

module EmparejamientoDeArboles where

-- | Tipo de árboles.
data Arbol a = N a [Arbol a]
  deriving (Show, Eq)

-- | Ejemplo de árbol.
ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- | Definición.
emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles f (N x l1) (N y l2) = 
  N (f x y) (zipWith (emparejaArboles f) l1 l2)
