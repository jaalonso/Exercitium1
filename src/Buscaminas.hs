-- Buscaminas.hs
-- Buscaminas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 15 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El buscaminas es un juego cuyo objetivo es despejar un campo de minas
-- sin detonar ninguna. 
-- 
-- El campo de minas se representa mediante un cuadrado con NxN
-- casillas. Algunas casillas tienen un número, este número indica las
-- minas que hay en todas las casillas vecinas. Cada casilla tiene como
-- máximo 8 vecinas. Por ejemplo, el campo 4x4 de la izquierda
-- contiene dos minas, cada una representada por el número 9, y a la 
-- derecha se muestra el campo obtenido anotando las minas vecinas de
-- cada casilla
--    9 0 0 0       9 1 0 0
--    0 0 0 0       2 2 1 0
--    0 9 0 0       1 9 1 0
--    0 0 0 0       1 1 1 0
-- de la misma forma, la anotación del siguiente a la izquierda es el de
-- la derecha 
--    9 9 0 0 0     9 9 1 0 0
--    0 0 0 0 0     3 3 2 0 0
--    0 9 0 0 0     1 9 1 0 0
--
-- Utilizando la librería Data.Matrix, los campos de minas se
-- representan mediante matrices: 
--    type Campo = Matrix Int
-- Por ejemplo, los anteriores campos de la izquierda se definen por
--    ejCampo1, ejCampo2 :: Campo
--    ejCampo1 = fromLists [[9,0,0,0],
--                          [0,0,0,0], 
--                          [0,9,0,0], 
--                          [0,0,0,0]]
--    ejCampo2 = fromLists [[9,9,0,0,0],
--                          [0,0,0,0,0],
--                          [0,9,0,0,0]]
-- 
-- Definir la función
--    buscaminas :: Campo -> Campo
-- tal que (buscaminas c) es el campo obtenido anotando las minas
-- vecinas de cada casilla. Por ejemplo,
--    ghci> buscaminas ejCampo1
--    ( 9 1 0 0 )
--    ( 2 2 1 0 )
--    ( 1 9 1 0 )
--    ( 1 1 1 0 )
--    
--    ghci> buscaminas ejCampo2
--    ( 9 9 1 0 0 )
--    ( 3 3 2 0 0 )
--    ( 1 9 1 0 0 )
-- Notas. 
-- 1. El manual de la librería Data.Matrix se encuentra en 
--    http://bit.ly/1hWVNJD 
-- 2. Las funciones de dicha librería útiles para este ejercicio son
--    fromLists, matrix, nrows y ncols. 
-- ---------------------------------------------------------------------

import Data.Matrix

type Campo   = Matrix Int
type Casilla = (Int,Int)

ejCampo1, ejCampo2 :: Campo
ejCampo1 = fromLists [[9,0,0,0],
                      [0,0,0,0], 
                      [0,9,0,0], 
                      [0,0,0,0]]
ejCampo2 = fromLists [[9,9,0,0,0],
                      [0,0,0,0,0],
                      [0,9,0,0,0]]

-- 1ª solución
-- ===========

buscaminas1 :: Campo -> Campo
buscaminas1 c = matrix m n (\(i,j) -> minas c (i,j))
    where m = nrows c
          n = ncols c

-- (minas c (i,j)) es el número de minas en las casillas vecinas de la
-- (i,j) en el campo de mina c y es 9 si en (i,j) hay una mina. Por
-- ejemplo,
--    minas ejCampo (1,1)  ==  9
--    minas ejCampo (1,2)  ==  1
--    minas ejCampo (1,3)  ==  0
--    minas ejCampo (2,1)  ==  2
minas :: Campo -> Casilla -> Int
minas c (i,j) 
    | c!(i,j) == 9 = 9
    | otherwise    = length (filter (==9) [c!(x,y) | (x,y) <- vecinas m n (i,j)])
                     where m = nrows c
                           n = ncols c

-- (vecinas m n (i,j)) es la lista de las casillas vecinas de la (i,j) en
-- un campo de dimensiones mxn. Por ejemplo,
--    vecinas 4 (1,1)  ==  [(1,2),(2,1),(2,2)]
--    vecinas 4 (1,2)  ==  [(1,1),(1,3),(2,1),(2,2),(2,3)]
--    vecinas 4 (2,3)  ==  [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
vecinas :: Int -> Int -> Casilla -> [Casilla]
vecinas m n (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                             b <- [max 1 (j-1)..min n (j+1)],
                             (a,b) /= (i,j)]

-- 2ª solución
-- ===========

buscaminas2 :: Campo -> Campo
buscaminas2 c = matrix m n (\(i,j) -> minas (i,j))
    where m = nrows c
          n = ncols c
          minas :: Casilla -> Int
          minas (i,j) 
              | c!(i,j) == 9 = 9
              | otherwise    = 
                  length (filter (==9) [c!(x,y) | (x,y) <- vecinas (i,j)])
          vecinas :: Casilla -> [Casilla]
          vecinas (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- David
-- =====

-- import Data.Matrix
-- 
-- type Campo = Matrix Int

buscaminasA1 :: Campo -> Campo
buscaminasA1 p = 
    matrix m n (\(i,j) -> if elem (i,j) indAyac && notElem (i,j) indices 
                          then length (filter (==(i,j)) indAyac) 
                          else p!(i,j))
    where 
      m = nrows p
      n = ncols p
      indices = [(i,j) | i <- [1..m], j <- [1..n], p!(i,j) /= 0]
      adyacentes (i,j) = [(k,l) | k <- [i-1..i+1], l <- [j-1..j+1]]
      indAyac = concat [adyacentes (i,j) | i<-[1..m], j <- [1..n], p!(i,j) /= 0]

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el problema [Minesweeper](http://bit.ly/1sfq0b8) de 
-- [UVa Online Judge](http://uva.onlinejudge.org).

