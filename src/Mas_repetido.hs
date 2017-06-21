-- Mas_repetido.hs
-- Elemento más repetido.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 1 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    masRepetido :: Ord a => [a] -> (a,Int)
-- tal que (masRepetido xs) es el elemento de xs que aparece más veces
-- de manera consecutiva en la lista junto con el número de sus
-- apariciones consecutivas; en caso de empate, se devuelve el último de
-- dichos elementos. Por ejemplo, 
--    masRepetido [1,1,4,4,1]  ==  (4,2)
--    masRepetido "aadda"      ==  ('d',2)
-- ---------------------------------------------------------------------

import Test.HUnit                     -- Para la verificación
import Data.List 
import Control.Arrow ((&&&))          -- Para la definición sin argumentos 
import Data.Function (on)
import Data.Tuple                     -- Para 6'

-- 1ª definición (por recursión):
masRepetido1 :: Ord a => [a] -> (a,Int)
masRepetido1 [x] = (x,1)
masRepetido1 (x:y:zs) | m > n     = (x,m)
                      | otherwise = (u,n)
    where (u,n) = masRepetido1 (y:zs)
          m     = length (takeWhile (==x) (x:y:zs))

-- 2ª definición (con group y maximum):
masRepetido2 :: Ord a => [a] -> (a,Int)
masRepetido2 xs = (n,z)
    where (z,n) = maximum [(length ys,y) | (y:ys) <- group xs]

-- 3ª definición (con group, maximum y swap):
masRepetidoP8 :: Ord a => [a] -> (a,Int)
masRepetidoP8 xs = 
    swap (maximum [(length ys,y) | (y:ys) <- group xs])

-- 4ª definición (con group y maximumBy):
masRepetido4 :: Ord a => [a] -> (a,Int)
masRepetido4 xs =  
    maximumBy compara [(y,length ys) | (y:ys) <- group xs]
    where compara (u,n) (v,m) = compare n m

-- 5ª definición (sin argumentos):
masRepetido5 :: Ord a => [a] -> (a,Int)
masRepetido5 = maximumBy (compare `on` snd) . map (head &&& length) . group

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Francisco Jácome Maura
masRepetidoP1 :: Ord a => [a] -> (a,Int)
masRepetidoP1 xs = aux2 (aux1 (agrupa xs))

-- Esta función hallará el máximo segundos elementos de los pares
-- que recorren una lista, y buscará el último elemento de esa lista tal
-- que su segundo elemento sea dicho máximo

aux2 xs = last (filter p xs)
    where p x = snd x == m
          m   = maximum (map snd xs)

-- Esta función auxiliar tomará los elementos y su número de
-- repeticiones de lo que se asume que es una
-- lista de listas de elementos repetidos

aux1 xss = [(head xs, length xs) | xs <- xss]

-- La siguiente función agrupa los elementos de una lista

agrupa [] = []
agrupa (x:xs) = takeWhile (==x) (x:xs) : agrupa (dropWhile (==x) (x:xs))

-- Luis
masRepetidoP2 :: Ord a => [a] -> (a,Int)
masRepetidoP2 xs = (x,y)
    where listas []     = []
          listas (x:xs) = (takeWhile (== x) (x:xs)):(listas (dropWhile (== x) (x:xs)))
          zs            = zip (listas xs) [length ys | ys <- listas xs]
          y             = maximum [n | (s,n) <- zs]
          x             = head (head [s | (s,n) <- reverse zs, n == y])

-- conjuagar (incorrecta)
emparejar xs = zip xs (tail xs)

repetidos xs = [(x,y)|(x,y)<-emparejar xs, x==y]

uno xs = tail(repetidos xs)

masRepetidoP3 xs = (fst(head (uno xs)),2)

-- David
masRepetidoP4 :: Ord a => [a] -> (a,Int)
masRepetidoP4 xs = last [(b,a) | (a,b) <- zs, a == longitud] 
 where zs           = [(length (y:ys),y) | (y:ys) <- consecutivos xs]
       (longitud,_) = maximum zs

consecutivos :: Eq a => [a] -> [[a]]
consecutivos []     = []
consecutivos (x:xs) = 
    takeWhile (==x) (x:xs):consecutivos (dropWhile (==x) xs) 

-- Laura
masRepetidoP5 :: Ord a => [a] -> (a,Int)
masRepetidoP5 xs =
    last ([(x,y) | (y,x) <- sort (zip (map (length) (gr xs)) (map (head) (gr xs)))])
    where gr xs = group xs

masRepetidoP6 :: Ord a => [a] -> (a,Int)
masRepetidoP6 xs = (x,y)
    where (y,x) = maximum (zip (map length ys) (map head ys))
              where ys = group xs

masRepetidoP7 :: Ord a => [a] -> (a,Int)
masRepetidoP7 xs = swap (maximum (zip (map length ys) (map head ys)))
    where ys = group xs

masRepetidoP8 :: Ord a => [a] -> (a,Int)
masRepetidoP8 xs = swap (maximum [(length ys, head ys)| ys <- group xs])

-- Angela
masRepetidoP9 :: Ord a => [a] -> (a,Int)
masRepetidoP9 xs = last [(head x,y) | (x,y) <- zip ys zs, y == maximum zs]
    where ys = group xs
          zs = map length ys

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

masRepetido :: Ord a => [a] -> (a,Int)
masRepetido = masRepetidoP9

ejemplos :: Test
ejemplos =
    test ["1" ~: "masRepetido [1,1,4,4,1]" ~: 
          masRepetido [1,1,4,4,1]  ~?=  (4,2),
          "2" ~: "masRepetido \"aadda\"" ~: 
          masRepetido "aadda"      ~?=  ('d',2)]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
