-- |
-- Module      : Pares_adyacentes_iguales.hs
-- Description : Número de pares de elementos adyacentes iguales en una matriz.
-- Copyright   : Exercitium (21-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Definir la función
-- 
-- > numeroParesAdyacentesIguales :: Eq a => [[a]] -> Int
-- 
-- tal que __(numeroParesAdyacentesIguales xss)__ es el número de pares de
-- elementos consecutivos (en la misma fila o columna) iguales de la
-- matriz xss. Por ejemplo,
-- 
-- >>> numeroParesAdyacentesIguales [[0,1],[0,2]]
-- 1
-- >>> numeroParesAdyacentesIguales [[0,0],[1,2]]
-- 1
-- >>> numeroParesAdyacentesIguales [[0,1],[0,0]]
-- 2
-- >>> numeroParesAdyacentesIguales [[1,2],[1,4],[4,4]]
-- 3
-- >>> numeroParesAdyacentesIguales ["ab","aa"]
-- 2
-- >>> numeroParesAdyacentesIguales [[0,0,0],[0,0,0],[0,0,0]]
-- 12
-- >>> numeroParesAdyacentesIguales [[0,0,0],[0,1,0],[0,0,0]]
-- 8
-- >>> numeroParesAdyacentesIguales [[0,0,0,0],[0,0,0,0],[0,0,0,0::Int]]
-- 17

module Pares_adyacentes_iguales where

import Data.List (group,transpose)
import Data.Array 
import Test.QuickCheck

-- | 1ª definición (Por comprensión).
numeroParesAdyacentesIguales :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales xss =
    numeroParesAdyacentesIgualesFilas xss +
    numeroParesAdyacentesIgualesFilas (transpose xss)

-- | __(numeroParesAdyacentesIgualesFilas xss)__ es el número de pares de
-- elementos consecutivos (en la misma fila) iguales de la matriz
-- xss. Por ejemplo,
-- 
-- >>> numeroParesAdyacentesIgualesFilas [[0,0,1,0],[0,1,1,0],[0,1,0,1]]
-- 2
-- >>> numeroParesAdyacentesIgualesFilas ["0010","0110","0101"]
-- 2
numeroParesAdyacentesIgualesFilas :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas xss =
    sum [numeroParesAdyacentesIgualesFila xs | xs <- xss]

-- | __(numeroParesAdyacentesIgualesFila xs)__ es el número de pares de
-- elementos consecutivos de la lista xs. Por ejemplo,
--
-- >>> numeroParesAdyacentesIgualesFila "0010"
-- 1
-- >>> numeroParesAdyacentesIgualesFila "0110"
-- 1
-- >>> numeroParesAdyacentesIgualesFila "0101"
-- 0
numeroParesAdyacentesIgualesFila :: Eq a => [a] -> Int
numeroParesAdyacentesIgualesFila xs =
    length [(x,y) | (x,y) <- zip xs (tail xs), x == y]

-- | 2ª definición de 'numeroParesAdyacentesIgualesFilas' (con 'map').
numeroParesAdyacentesIgualesFilas2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas2 xss =
    sum (map numeroParesAdyacentesIgualesFila xss)

-- | 3ª definición de 'numeroParesAdyacentesIgualesFilas' (sin argumentos).
numeroParesAdyacentesIgualesFilas3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas3 =
    sum . (map numeroParesAdyacentesIgualesFila)

-- | Comprobación de la equivalencia de las definiciones de
-- numeroParesAdyacentesIgualesFilas.
--
-- >>> quickCheck prop_numeroParesAdyacentesIgualesFilas
-- +++ OK, passed 100 tests.
prop_numeroParesAdyacentesIgualesFilas :: [[Int]] -> Bool
prop_numeroParesAdyacentesIgualesFilas xss =
  all (== numeroParesAdyacentesIgualesFilas xss)
      [f xss | f <- [ numeroParesAdyacentesIgualesFilas2
                    , numeroParesAdyacentesIgualesFilas3
                    ]]

-- | 2ª definición de 'numeroParesAdyacentesIguales' (Por composición).
numeroParesAdyacentesIguales2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales2 xss =
    length (concatMap tail (concatMap group (xss ++ transpose xss)))

-- | 3ª definición de 'numeroParesAdyacentesIguales' (con matrices)
numeroParesAdyacentesIguales3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales3 xss =
  length [(i,j) | i <- [1..n-1], j <- [1..m], p!(i,j) == p!(i+1,j)] + 
  length [(i,j) | i <- [1..n], j <- [1..m-1], p!(i,j) == p!(i,j+1)]
  where n = length xss
        m = length (head xss)
        p = listArray ((1,1),(n,m)) (concat xss)

-- | 4ª definición de 'numeroParesAdyacentesIguales' (sin argumentos)
numeroParesAdyacentesIguales4 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales4 =
  length . (tail =<<) . (group =<<) . ((++) =<< transpose)

-- | Tipo de matrices.
newtype Matriz = M [[Int]]
  deriving Show

-- | __matriz__ es un generador de matrices. Por ejemplo,
--
-- > > sample matriz
-- > M [[0,0]]
-- > M [[1,-2,-2],[-2,2,0],[2,-1,-2],[-2,0,-2]]
-- > M [[3,-4,0,0,2],[2,2,0,-2,2],[-1,-4,2,-3,0],[0,4,-1,-2,-3],[-3,-4,0,-4,-2]]
-- > M [[1,3],[2,-3],[0,3],[-3,2]]
-- > M [[-3,-3,-4,2,-6,0],[5,-6,3,-2,2,-8],[-6,8,5,1,0,-7],[7,3,7,-8,-3,-2]]
-- > M [[-9,7,0]]
-- > M [[8,-1,11,8,-13,9],[12,13,12,1,0,-9],[3,-4,7,-1,5,-2]]
-- > M [[-3,16,-9,-14,15,-6],[7,-14,8,-10,7,2]]
matriz :: Gen Matriz
matriz = do
  n <- choose (1,6)
  m <- choose (1,6)
  xs <- vectorOf (n*m) arbitrary
  return (M (listaAmatriz m xs))

-- | __(listaAmatriz m xs)__ es la matriz con m columnas cuyos elementos
-- son los de xs. Por ejemplo,
--
-- >>> listaAmatriz 3 [2,1,5,4,7,9]
-- [[2,1,5],[4,7,9]]
-- >>> listaAmatriz 2 [2,1,5,4,7,9]
-- [[2,1],[5,4],[7,9]]
listaAmatriz :: Int -> [a] -> [[a]]
listaAmatriz _ [] = []
listaAmatriz m xs = take m xs : listaAmatriz m (drop m xs)

-- | __(prop_numeroParesAdyacentesIguales (M xss))__ se verifica si las
-- definiciones de numeroParesAdyacentesIguales son equivalentes sobre
-- xss. Por ejemplo,
--
-- >>> :{
--  all prop_numeroParesAdyacentesIguales
--      [M [[0,1],[0,2]],
--       M [[0,0],[1,2]],
--       M [[0,1],[0,0]],
--       M [[1,2],[1,4],[4,4]],
--       M [[0,0,0],[0,0,0],[0,0,0]],
--       M [[0,0,0],[0,1,0],[0,0,0]],
--       M [[0,0,0,0],[0,0,0,0],[0,0,0,0]]]
-- :}
-- True

prop_numeroParesAdyacentesIguales :: Matriz -> Bool
prop_numeroParesAdyacentesIguales (M xss) =
  all (== numeroParesAdyacentesIguales xss)
      [f xss | f <- [ numeroParesAdyacentesIguales2
                    , numeroParesAdyacentesIguales3
                    , numeroParesAdyacentesIguales4
                    ]]

-- | Comprueba la equivalencia de todas las definiciones de
-- numeroParesAdyacentesIguales.
-- 
-- >>> verifica_numeroParesAdyacentesIguales
-- +++ OK, passed 100 tests.
verifica_numeroParesAdyacentesIguales :: IO ()
verifica_numeroParesAdyacentesIguales =
    quickCheck (forAll matriz prop_numeroParesAdyacentesIguales)
