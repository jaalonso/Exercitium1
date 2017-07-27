-- |
-- Module      : Amplia_columnas
-- Description : Ampliación de las columnas de una matriz.
-- Copyright   : Exercitium (16-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Las matrices enteras se pueden representar mediante tablas con
-- índices enteros:
--
-- >   type Matriz = Array (Int,Int) Int
-- 
-- Definir la función
-- 
-- >   ampliaColumnas :: Matriz -> Matriz -> Matriz
-- 
-- tal que __(ampliaColumnas p q)__ es la matriz construida añadiendo las
-- columnas de la matriz q a continuación de las de p (se supone que
-- tienen el mismo número de filas). Por ejemplo, si p y q representan
-- las dos primeras matrices, entonces (ampliaColumnas p q) es la
-- tercera
-- 
-- >   |0 1|    |4 5 6|    |0 1 4 5 6| 
-- >   |2 3|    |7 8 9|    |2 3 7 8 9|
-- 
-- En Haskell,
--
-- >>> let p = listArray ((1,1),(2,2)) [0..3]
-- >>> let q = listArray ((1,1),(2,3)) [4..9]
-- >>> elems (ampliaColumnas p q)
-- [0,1,4,5,6,2,3,7,8,9]
-- >>> bounds (ampliaColumnas p q)
-- ((1,1),(2,5))

module Amplia_columnas where

import Data.Array

-- | Tipo de las matrices.
type Matriz = Array (Int,Int) Int

-- | Definición.
ampliaColumnas :: Matriz -> Matriz -> Matriz
ampliaColumnas p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
    where ((_,_),(m,n1)) = bounds p1
          ((_,_),(_,n2)) = bounds p2
          f i j | j <= n1   = p1!(i,j)
                | otherwise = p2!(i,j-n1) 
