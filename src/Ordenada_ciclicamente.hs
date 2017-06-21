-- Ordenada_ciclicamente.hs
-- Ordenada cíclicamente.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 7 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se dice que una sucesión x(1), ..., x(n) está ordenada cíclicamente
-- si existe un índice i tal que la sucesión  
--    x(i), x(i+1), ..., x(n), x(1), ..., x(i-1)
-- está ordenada crecientemente. 
-- 
-- Definir la función 
--    ordenadaCiclicamente :: Ord a => [a] -> Int
-- tal que (ordenadaCiclicamente xs) es el índice (empezando en 1) a
-- partir del cual está ordenada, si el la lista está ordenado cíclicamente
-- y 0 en caso contrario. Por ejemplo,
--    ordenadaCiclicamente [1,2,3,4]      ==  1  
--    ordenadaCiclicamente [5,8,2,3]      ==  3 
--    ordenadaCiclicamente [4,6,7,5,4,3]  ==  0 
--    ordenadaCiclicamente [1,0,1,2]      ==  0
--    ordenadaCiclicamente [0,2,0]        ==  3
--    ordenadaCiclicamente "cdeab"        ==  4
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List 

-- 1ª definición (por comprensión)
-- ===============================

ordenadaCiclicamente1 :: Ord a => [a] -> Int
ordenadaCiclicamente1 xs = 
    primero [n+1 | n <- [0..length xs-1], 
                   ordenada (drop n xs ++ take n xs)]
    where primero []     = 0
          primero (x:xs) = x

-- (ordenada xs) se verifica si la lista xs está ordenada
-- crecientemente. Por ejemplo,
--   ordenada "acd"   ==  True
--   ordenada "acdb"  ==  False
ordenada :: Ord a => [a] -> Bool 
ordenada (x:y:zs) = x <= y && ordenada (y:zs) 
ordenada _        = True 
 
-- 2ª definición (por recursión)
-- =============================

ordenadaCiclicamente2 :: Ord a => [a] -> Int
ordenadaCiclicamente2 xs = aux xs 1 (length xs)  
    where aux xs i k 
              | i > k       = 0 
              | ordenada xs = i 
              | otherwise   = aux (siguienteCiclo xs) (i+1) k 

-- (siguienteCiclo xs) es la lista obtenida añadiendo el primer elemento
-- de xs al final del resto de xs. Por ejemplo,
--   siguienteCiclo [3,2,5]  =>  [2,5,3]
siguienteCiclo [] = [] 
siguienteCiclo (x:xs) = xs ++ [x] 
