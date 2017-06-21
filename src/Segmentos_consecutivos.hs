-- Segmentos_consecutivos.hs
-- Segmentos de elementos consecutivos.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 30 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    segmentos :: (Enum a, Eq a) => [a] -> [[a]]
-- tal que (segmentos xss) es la lista de los segmentos de xss formados
-- por elementos consecutivos. Por ejemplo,
--    segmentos [1,2,5,6,4]     ==  [[1,2],[5,6],[4]]
--    segmentos [1,2,3,4,7,8,9] ==  [[1,2,3,4],[7,8,9]]
--    segmentos "abbccddeeebc"  ==  ["ab","bc","cd","de","e","e","bc"]
-- Nota: Se puede usar la función succ tal que (succ x) es el sucesor de
-- x. Por ejemplo,
--    succ 3    ==  4
--    succ 'c'  ==  'd'
-- ---------------------------------------------------------------------

import Test.HUnit
import Test.QuickCheck

-- 1ª definición (por recursión):
segmentos1 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos1 []  = []
segmentos1 [x] = [[x]]
segmentos1 (x:xs) | y == succ x = (x:y:ys):zs
                  | otherwise   = [x] : (y:ys):zs
    where ((y:ys):zs) = segmentos1 xs

-- 2ª definición.
segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
    where ys = inicial xs
          n  = length ys
          zs = drop n xs

-- (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
--    inicial [1,2,5,6,4]  ==  [1,2]
--    inicial "abccddeeebc"  ==  "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial [] = []
inicial (x:xs) = 
    [y | (y,z) <- takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..])]
