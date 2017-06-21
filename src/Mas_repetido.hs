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
