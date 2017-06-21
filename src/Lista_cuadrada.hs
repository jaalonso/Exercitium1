-- Lista_cuadrada.hs
-- Lista cuadrada.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  3 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    listaCuadrada :: Int -> a -> [a] -> [[a]] 
-- tal que (listaCuadrada n x xs) es una lista de n listas de longitud n
-- formadas con los elementos de xs completada con x, si no xs no tiene
-- suficientes elementos. Por ejemplo,
--    listaCuadrada 3 7 [0,3,5,2,4]  ==  [[0,3,5],[2,4,7],[7,7,7]]
--    listaCuadrada 3 7 [0..]        ==  [[0,1,2],[3,4,5],[6,7,8]]
--    listaCuadrada 2 'p' "eva"      ==  ["ev","ap"]
--    listaCuadrada 2 'p' ['a'..]    ==  ["ab","cd"]
-- ---------------------------------------------------------------------

import Test.HUnit     
import Test.QuickCheck

-- 1ª definición (por recursión):
listaCuadrada1 :: Int -> a -> [a] -> [[a]] 
listaCuadrada1 n x xs =
    take n (grupos n (xs ++ repeat x))

-- (grupos n xs) es la lista obtenida agrupando los elementos de xs en
-- grupos de n elementos, salvo el último que puede tener menos. Por
-- ejemplo, 
--    grupos 2 [4,2,5,7,6]     ==  [[4,2],[5,7],[6]]
--    take 3 (grupos 3 [1..])  ==  [[1,2,3],[4,5,6],[7,8,9]]
grupos :: Int -> [a] -> [[a]]
grupos _ [] = []
grupos n xs = take n xs : grupos n (drop n xs)

-- 2ª definición (por comprensión)
listaCuadrada2 :: Int -> a -> [a] -> [[a]]
listaCuadrada2 n x xs = 
    take n [take n ys | m <- [0,n..n^2],
                        ys <- [drop m xs ++ (replicate m x)]]

-- 3ª definición (por iteración):
listaCuadrada3 :: Int -> a -> [a] -> [[a]] 
listaCuadrada3 n x xs =
    take n [take n ys | ys <- iterate (drop n) (xs ++ repeat x)]

-- 4ª definición (sin el 4º argumento):
listaCuadrada4 :: Int -> a -> [a] -> [[a]] 
listaCuadrada4 n x = 
    take n . map (take n) . iterate (drop n) . (++ repeat x)
