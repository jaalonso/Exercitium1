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

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- luiporpir (incorrecta)
segmentosA1 :: (Enum a, Eq a) => [a] -> [[a]]
segmentosA1 []  = []
segmentosA1 [x] = [[x]]
segmentosA1 (y:x:xs) | succ y == x = [y,x] : segmentosA1 xs
                     | otherwise   = [y] : segmentosA1 (x:xs)

-- Contraejemplo:
--    ghci> segmentosA1 [1..5]
--    [[1,2],[3,4],[5]]

-- Eduardo
segmentosA2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentosA2 [] = []
segmentosA2 [x] = [[x]]
segmentosA2 (x:(y:xs)) | y == succ x = [[x,y]]++ segmentosA2 xs
                       | x == y = [[x]]++[[x]]++segmentosA2 xs
                       | otherwise = segmentosA2 xs

-- Contraejemplo:
--    ghci> segmentosA2 [1..5]
--    [[1,2],[3,4],[5]]

-- Eduardo
segmentosA3 :: (Enum a, Eq a) => [a] -> [[a]]
segmentosA3 [] = []
segmentosA3 xs = [aux xs] ++ segmentosA3 (drop (length (aux xs)) xs)

aux [x] = [x]
aux (x:(y:xs)) | y == succ x = [x] ++ aux (y:xs)
               | otherwise = [x]

-- Eduardo
segmentosA4 :: (Enum a, Eq a) => [a] -> [[a]]
segmentosA4 [] = []
segmentosA4 xs = 
    [segmentoInicial xs] ++ segmentosA4 (drop (length (segmentoInicial xs)) xs)

-- segmentoInicial xs : obtiene el primer segmento de mayor longitud formado por
-- elementos consecutivos.
segmentoInicial :: (Enum a, Eq a) => [a] -> [a]
segmentoInicial [x] = [x]
segmentoInicial (x:(y:xs)) | y == succ x = (x:segmentoInicial (y:xs))
                           | otherwise   = [x]

-- David
segmentosA5 :: (Enum a, Eq a) => [a] -> [[a]]
segmentosA5 [] = []
segmentosA5 xss = m : segmentosA5 (drop (length m) xss)
    where m = segmento [] xss

segmento ys [x] = ys ++ [x]
segmento ys (x:y:yss) | succ x == y = segmento (ys++[x]) (y:yss)
                      | otherwise = ys++[x]

-- Laura
segmentosA6 :: (Enum a, Eq a) => [a] -> [[a]]
segmentosA6 []     = []
segmentosA6 (x:xs) = aux (x:xs) [x] []
    where aux [ ] ac1 ac2 = ac2 ++ [ac1]
          aux [x] ac1 ac2 = aux [] ac1 ac2
          aux (x:y:ys) ac1 ac2 | succ x == y = aux (y:ys) (ac1 ++ [y]) ac2
                               | otherwise = aux (y:ys) [y] (ac2 ++ [ac1])

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

segmentos :: (Enum a, Eq a) => [a] -> [[a]]
segmentos = segmentosA5

ejemplos :: Test
ejemplos =
    test ["1" ~: "segmentos [1,2,5,6,4]" ~: 
          segmentos [1,2,5,6,4]     ~?=  [[1,2],[5,6],[4]],
          "2" ~: "segmentos [1,2,3,4,7,8,9]" ~: 
          segmentos [1,2,3,4,7,8,9] ~?=  [[1,2,3,4],[7,8,9]],
          "3" ~: "segmentos \"abbccddeeebc\"" ~: 
          segmentos "abbccddeeebc"  ~?=  ["ab","bc","cd","de","e","e","bc"]]

verifica = runTestTT ejemplos

prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
    segmentos1 xs == segmentosA6 xs

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
