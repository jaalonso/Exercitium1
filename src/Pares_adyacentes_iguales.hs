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

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
numeroParesAdyacentesIgualesA1 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesA1 xss = 
    sum [numIgPorFil xs | xs <- xss] + 
    sum [numIgPorFil xs | xs <- column xss]
    where numIgPorFil [_] = 0
          numIgPorFil (x:xs) | x == head xs = 1 + numIgPorFil xs
                             | otherwise    = numIgPorFil xs
          column [] = []
          column (xs:xss) | xs == []  = []
                          | otherwise = [head ys | ys <- xs:xss] :
                                        (column [tail ys | ys <- xs:xss])

-- Agustín
numeroParesAdyacentesIgualesA2 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesA2 [[]] = 0
numeroParesAdyacentesIgualesA2 xss =
    adyPorFilas xss + adyPorFilas (transpose xss)

adyPorFilas xss = sum [1 | xs <- xss, (a,b) <- zip xs (tail xs), a == b]

-- Laura
numeroParesAdyacentesIgualesA3 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesA3 xss =
    sum [1 | (x,y) <- consecMismaFil xss ++ consecMismaCol xss, x == y]
    where consecMismaFil xss = concat [zip xs (tail xs) | xs <- xss]
          consecMismaCol xss = concat [zip xs (tail xs) | xs <- transpose xss]

-- David
numeroParesAdyacentesIgualesA4 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesA4 xss = 
    length [(a,b) | (a,b) <- xs++ys, a == b]
    where (m,n) = (length xss, length (head xss))
          p     = listArray ((1,1),(m,n)) (concat xss)
          xs    = [(p!(i,j),p!(i,j+1)) | i <- [1..m], j <- [1..n], j+1 <= n]
          ys    = [(p!(i,j),p!(i+1,j)) | i <- [1..m], j <- [1..n], i+1 <= m]

-- Ángela
numeroParesAdyacentesIgualesA5 :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesA5 xss = aux xss + aux (transpose xss)

aux xs = sum (map f [ys | ys <- zs, length ys > 1])
    where zs   = (concat[ys | ys <- (map group xs)])
          f xs = length [(x,y) | (x,y) <- zip (tail xs) xs]  


numeroParesAdyacentesIgualesA5b :: Eq a => [[a]] -> Int
numeroParesAdyacentesIgualesA5b xss = aux xss + aux (transpose xss)
    where aux xs = sum (map f zs)
              where zs   = concat [ys | ys <- map group xs]
                    f xs = length (tail xs)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

numeroParesAdyacentesIguales :: Eq a => [[a]] -> Int
numeroParesAdyacentesIguales = numeroParesAdyacentesIgualesA5b

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~: 
          numeroParesAdyacentesIguales [[0,1],[0,2]]               ~?=  1,
          "2" ~: "ej2" ~: 
          numeroParesAdyacentesIguales [[0,0],[1,2]]               ~?=  1,
          "3" ~: "ej3" ~: 
          numeroParesAdyacentesIguales [[0,1],[0,0]]               ~?=  2,
          "4" ~: "ej3" ~: 
          numeroParesAdyacentesIguales ["ab","aa"]                 ~?=  2,
          "5" ~: "ej5" ~: 
          numeroParesAdyacentesIguales [[0,0,0],[0,0,0],[0,0,0]]   ~?=  12,
          "6" ~: "ej6" ~: 
          numeroParesAdyacentesIguales [[0,0,0],[0,1,0],[0,0,0]]   ~?=   8,
          "7" ~: "ej7" ~: 
          numeroParesAdyacentesIguales [[1,2],[1,4],[4,4]]         ~?=  3]
                                  

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
