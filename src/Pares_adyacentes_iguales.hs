-- Pares_adyacentes_iguales.hs
-- Número de pares de elementos adyacentes iguales en una matriz.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 17 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    numeroParesAdyacentesIguales :: Eq a => [[a]] -> Int
-- tal que (numeroParesAdyacentesIguales xss) es el número de pares de
-- elementos consecutivos (en la misma fila o columna) iguales de la
-- matriz xss. Por ejemplo,
--    numeroParesAdyacentesIguales [[0,1],[0,2]]              ==  1
--    numeroParesAdyacentesIguales [[0,0],[1,2]]              ==  1
--    numeroParesAdyacentesIguales [[0,1],[0,0]]              ==  2
--    numeroParesAdyacentesIguales [[1,2],[1,4],[4,4]]        ==  3
--    numeroParesAdyacentesIguales ["ab","aa"]                ==  2
--    numeroParesAdyacentesIguales [[0,0,0],[0,0,0],[0,0,0]]  ==  12
--    numeroParesAdyacentesIguales [[0,0,0],[0,1,0],[0,0,0]]  ==  8
-- ---------------------------------------------------------------------

import Data.List (group,transpose)
import Data.Array
import Test.HUnit

-- ---------------------------------------------------------------------
-- § 1ª solución (Por comprensión)
-- ---------------------------------------------------------------------

numeroParesAdyacentesIguales1 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales1 xss =
    numeroParesAdyacentesIgualesFilas xss +
    numeroParesAdyacentesIgualesFilas (transpose xss)

-- (numeroParesAdyacentesIgualesFilas xss) es el número de pares de
-- elementos consecutivos (en la misma fila) iguales de la matriz
-- xss. Por ejemplo, 
--    ghci> numeroParesAdyacentesIgualesFilas [[0,0,1,0],[0,1,1,0],[0,1,0,1]]
--    2
--    ghci> numeroParesAdyacentesIgualesFilas ["0010","0110","0101"]
--    2
numeroParesAdyacentesIgualesFilas :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas xss =
    sum [numeroParesAdyacentesIgualesFila xs | xs <- xss]

-- La función anterior se puede definir con map
numeroParesAdyacentesIgualesFilas2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas2 xss =
    sum (map numeroParesAdyacentesIgualesFila xss)

-- y también se puede definir sin argumentos:
numeroParesAdyacentesIgualesFilas3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesFilas3 =
    sum . (map numeroParesAdyacentesIgualesFila)

-- (numeroParesAdyacentesIgualesFila xs) es el número de pares de
-- elementos consecutivos de la lista xs. Por ejemplo, 
numeroParesAdyacentesIgualesFila :: Eq a => [a] -> Int
numeroParesAdyacentesIgualesFila xs =
    length [(x,y) | (x,y) <- zip xs (tail xs), x == y]

-- ---------------------------------------------------------------------
-- § 2ª solución (Por composición)
-- ---------------------------------------------------------------------

-- numeroParesAdyacentesIguales2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales2 xss =
    length (concatMap tail (concatMap group (xss ++ transpose xss)))

-- ---------------------------------------------------------------------
-- § 3ª solución (con matrices)
-- ---------------------------------------------------------------------

numeroParesAdyacentesIguales3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales3 xss =
    length [(i,j) | i <- [1..n-1], j <- [1..m], p!(i,j) == p!(i+1,j)] + 
    length [(i,j) | i <- [1..n], j <- [1..m-1], p!(i,j) == p!(i,j+1)]
    where m = length xss
          n = length (head xss)
          p = listArray ((1,1),(m,n)) (concat xss)

-- ---------------------------------------------------------------------
-- § 4ª solución (sin argumentos)
-- ---------------------------------------------------------------------

numeroParesAdyacentesIguales4 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales4 =
    length . (tail =<<) . (group =<<) . ((++) =<< transpose)
