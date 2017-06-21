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
--    9000      9100
--    0000      2210
--    0900      1910
--    0000      1110
-- de la misma forma, la anotación del siguiente a la izquierda es el de
-- la derecha 
--    99000     99100
--    00000     33200
--    09000     19100
--
-- Los campos de minas se representan mediante matrices:
--    type Campo = Array (Int,Int) Int
-- Por ejemplo, los anteriores campos de la izquierda se definen por
--    ejCampo1, ejCampo2 :: Campo
--    ejCampo1 = listArray ((1,1),(4,4)) [9,0,0,0,
--                                        0,0,0,0, 
--                                        0,9,0,0, 
--                                        0,0,0,0]
--    ejCampo2 = listArray ((1,1),(3,5)) [9,9,0,0,0,
--                                        0,0,0,0,0,
--                                        0,9,0,0,0]
-- 
-- Definir la función
--    buscaminas :: Campo -> Campo
-- tal que (buscaminas c) es el campo obtenido anotando las minas
-- vecinas de cada casilla. Por ejemplo,
--    ghci> buscaminas ejCampo1
--    array ((1,1),(4,4)) [((1,1),9),((1,2),1),((1,3),0),((1,4),0),
--                         ((2,1),2),((2,2),2),((2,3),1),((2,4),0),
--                         ((3,1),1),((3,2),9),((3,3),1),((3,4),0),
--                         ((4,1),1),((4,2),1),((4,3),1),((4,4),0)]
--    ghci> buscaminas1 ejCampo2
--    array ((1,1),(3,5)) [((1,1),9),((1,2),9),((1,3),1),((1,4),0),((1,5),0),
--                         ((2,1),3),((2,2),3),((2,3),2),((2,4),0),((2,5),0),
--                         ((3,1),1),((3,2),9),((3,3),1),((3,4),0),((3,5),0)]
-- ---------------------------------------------------------------------

import Data.Array

type Campo   = Array (Int,Int) Int
type Casilla = (Int,Int)

ejCampo1, ejCampo2 :: Campo
ejCampo1 = listArray ((1,1),(4,4)) [9,0,0,0,
                                    0,0,0,0, 
                                    0,9,0,0, 
                                    0,0,0,0]
ejCampo2 = listArray ((1,1),(3,5)) [9,9,0,0,0,
                                    0,0,0,0,0,
                                    0,9,0,0,0]

-- 1ª solución
-- ===========

buscaminas1 :: Campo -> Campo
buscaminas1 c = 
    array ((1,1),(m,n)) [((i,j),minas c (i,j)) | i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds c               

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
                     where (_,(m,n)) = bounds c               

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
buscaminas2 c = 
    array ((1,1),(m,n)) [((i,j),minas (i,j)) | i <- [1..n], j <- [1..n]]
    where (_,(m,n)) = bounds c               
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
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el problema [Minesweeper](http://bit.ly/1sfq0b8) de 
-- [UVa Online Judge](http://uva.onlinejudge.org).

