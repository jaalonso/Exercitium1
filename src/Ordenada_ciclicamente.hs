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

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis (incorrecta)
-- =================

ordenadaCiclicamenteA1 :: Ord a => [a] -> Int
ordenadaCiclicamenteA1 xs 
    | minimum xs == last xs = 0
    | otherwise             = 1 + length (takeWhile (> minimum xs) xs)

-- Contraejemplo
--    ghci> ordenadaCiclicamenteA1 [3,4,2]
--    0
--    ghci> ordenadaCiclicamente1 [3,4,2]
--    3

ordenadaCiclicamenteA2 xs 
    | ys == sort ys && length ys > 1 = 1 + length (takeWhile (/= minimum xs) xs)
    | otherwise                      = 0
    where ys = dropWhile (/= minimum xs) xs

-- Contraejemplo:
--    ghci> ordenadaCiclicamenteA2 [1,0,1,2]
--    2
--    ghci> ordenadaCiclicamente1 [1,0,1,2]
--    0

-- Alberto (incorrecta)
-- ====================

ordenadaCiclicamenteA3 [] = 0
ordenadaCiclicamenteA3 xs 
    | (todosFalse (listaord2(todoscambia xs)))==True = 0
    | otherwise = posicion (listaord2(todoscambia xs))

todosFalse [] = True
todosFalse (x:xs) | x == False = todosFalse xs
                  | otherwise  = False

listaord [] = True
listaord [x] = True
listaord xs | head xs < head (tail xs) = listaord (tail xs)
            | otherwise = False

posicion [] = 0
posicion xs | head xs == True = 1
            | otherwise       = posicion (tail xs) + 1

listaord2 xss = [listaord xs|xs<-xss]

cambia n []     = []
cambia 1 xs     = xs
cambia n (x:xs) = cambia (n-1) (xs ++ [x])

todoscambia xs = [cambia n xs|n<-[1..length xs]]

-- Luis
-- ====

ordenadaCiclicamenteA4 xs 
    | ys == sort ys = 1 + length (fst (span (>(minimum xs)) xs))                     
    | otherwise = 0
    where ys = snd (span (>(minimum xs)) xs) ++ fst (span (>(minimum xs)) xs)

ordenadaCiclicamenteA4a xs 
    | ys == sort ys = 1 + length as
    | otherwise = 0
    where m       = minimum xs
          (as,bs) = span (>m) xs    
          ys      = bs ++ as

-- Laura
-- =====
ordenadaCiclicamenteA5 xs = aux xs 1 1
    where aux [x] ac1 ac2 = ac1
          aux [x,y] ac1 ac2 | x /= pred y = 0
                            | otherwise   = ac1
          aux (x:y:xs) ac1 ac2 | x == pred y = aux (y:xs) ac1 (ac2+1)
                               | otherwise   = aux (y:xs) (ac2+1) (ac2+1)

-- Eduardo
-- =======
ordenadaCiclicamenteA6 xs = 
    if   (ordenadaCiclicamenteA61 (length xs) xs) <= (length xs)  
    then ordenadaCiclicamenteA61 (length xs) xs 
    else 0

ordenadaCiclicamenteA61 0 xs = length xs
ordenadaCiclicamenteA61 n ys@(x:xs) 
    | ordenadaA6 ys = 1
    | otherwise   = 1 + ordenadaCiclicamenteA61 (n-1) (xs ++ [x])

ordenadaA6 [x] = True
ordenadaA6 (x:y:xs) | x <= y    = ordenadaA6 (y:xs)
                    | otherwise = False

-- Paco Jácome
-- ===========

ordenadaCiclicamenteA7 :: Ord a => [a] -> Int
ordenadaCiclicamenteA7 xs
    | or ys     = length (takeWhile (==False) ys) + 1
    | otherwise = 0
    where ys = [reordena n xs == sort xs | n <- [1..length xs]]

reordena n xs = drop (n-1) xs ++ take (n-1) xs

-- David
-- =====
ordenadaCiclicamenteA8 :: Ord a => [a] -> Int      
ordenadaCiclicamenteA8 xs = mod  y (length xs)  
    where (x,y) = minimum [(x,y)| (x,y) <- zip xs [1..]]

-- Loles Valverde
-- ==============

ordenadaCiclicamenteA9 :: Ord a => [a] -> Int
ordenadaCiclicamenteA9 xs = ordenadaA9 1 xs

ordenadaA9 :: Ord a => Int -> [a] -> Int
ordenadaA9 n (x:xs) | n>length (x:xs) = 0
                    | sort (x:xs) == (x:xs) = n
                    | otherwise = ordenadaA9 (n+1) (xs++[x])

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

-- Las definiciones son equivalentes
prop_ordenadaCiclicamente :: [Int] -> Property
prop_ordenadaCiclicamente xs =
    length (nub xs) > 1 ==>
    ordenadaCiclicamente1 xs == ordenadaCiclicamenteA8 xs

-- La comprobación es
--    ghci> quickCheck prop_ordenadaCiclicamente
--    +++ OK, passed 100 tests.

-- Las dos definiciones son igualmente eficientes:
--    ghci> :set +s 
--    ghci> ordenadaCiclicamente1 ([2..1000] ++ [1])
--    1000
--    (0.65 secs, 79333400 bytes)
--    ghci> ordenadaCiclicamente2 ([2..1000] ++ [1])
--    1000
--    (0.66 secs, 91602380 bytes)
--    ghci> :unset +s
