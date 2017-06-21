-- Mas_cercano_cumpliendo_la_propiedad.hs
-- Elemento más cercano que cumple una propiedad.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  5 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    cercano :: (a -> Bool) -> Int -> [a] -> Maybe a
-- tal que (cercano p n xs) es el elemento de p más cercano a n que
-- verifica la propiedad p. La búsqueda comienza en n y los elementos se
-- analizan en el siguiente orden: n, n+1, n-1, n+2, n-2,... Por ejemplo, 
--    cercano (`elem` "aeiou") 6 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") 1 "Sevilla"     ==  Just 'e'
--    cercano (`elem` "aeiou") 2 "Sevilla"     ==  Just 'i'
--    cercano (`elem` "aeiou") 5 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") 9 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") (-3) "Sevilla"  ==  Just 'e'
--    cercano (>100) 4 [200,1,150,2,4]         ==  Just 150
--    cercano even 5 [1,3..99]                 ==  Nothing
-- ---------------------------------------------------------------------

import Data.List
import Data.Maybe
import Control.Arrow
import Control.Applicative

-- ---------------------------------------------------------------------
-- § Soluciones elementales                                           --
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

cercano1 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano1 p n xs | null ys   = Nothing
                | otherwise = Just (head ys)
    where ys = filter p (ordenaPorCercanos xs n)

-- (ordenaPorCercanos xs n) es la lista de los elementos de xs que
-- ocupan las posiciones n, n+1, n-1, n+2, n-2... Por ejemplo, 
--    ordenaPorCercanos [0..9] 4     ==  [4,5,3,6,2,7,1,8,0,9]
--    ordenaPorCercanos [0..9] 7     ==  [7,8,6,9,5,4,3,2,1,0]
--    ordenaPorCercanos [0..9] 2     ==  [2,3,1,4,0,5,6,7,8,9]
--    ordenaPorCercanos [0..9] (-3)  ==  [0,1,2,3,4,5,6,7,8,9]
--    ordenaPorCercanos [0..9] 20    ==  [9,8,7,6,5,4,3,2,1,0]
ordenaPorCercanos :: [a] -> Int -> [a]
ordenaPorCercanos xs n 
    | n < 0          = xs
    | n >= length xs = reverse xs
    | otherwise      = z : intercala zs (reverse ys)
    where (ys,(z:zs)) = splitAt n xs

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- las lista xs e ys. Por ejemplo,
--    intercala [1..4] [5..10]   ==  [1,5,2,6,3,7,4,8,9,10]
--    intercala [5..10] [1..4]   ==  [5,1,6,2,7,3,8,4,9,10]
intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 2ª solución (usando find)
-- =========================

cercano2 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano2 p n xs = find p (ordenaPorCercanos xs n)

-- ---------------------------------------------------------------------
-- § Soluciones avanzadas                                             --
-- ---------------------------------------------------------------------

cercanoA1 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercanoA1 p n = 
    find p . concat . transpose . parAlista . first reverse . splitAt n

-- (parAlista p) es la lista correspondiente al par p.Por ejemplo,
--    pairToList (3,5)  ==  [3,5]

parAlista :: (a,a) -> [a]
parAlista (x,y) = [x,y]

-- ---------------------------------------------------------------------
-- § Soluciones en comentarios                                        --
-- ---------------------------------------------------------------------

-- María Ruiz
-- ==========

cercanoC1 :: (Eq a) => (a -> Bool) -> Int -> [a] -> Maybe a
cercanoC1 p n xs | null zs   = Nothing
                 | otherwise = head zs
    where m = length xs
          ps = posiciones n
          ks = menorSegContiene 0 (m-1) ps
          zs = [y | y <- seleccionaSegunPosicion ks xs, 
                    y /= Nothing, let Just z = y in p z]
 
posiciones :: Int -> [Int]
posiciones n = n:concat[[n+k,n-k] | k <-[1..]]
 
menorSegContiene :: Eq a => a -> a -> [a] -> [a]
menorSegContiene x y xs =
    head [ys | ys <-inits xs, elem x ys, elem y ys]
 
seleccionaSegunPosicion :: [Int] -> [a] -> [Maybe a]
seleccionaSegunPosicion [] xs = []
seleccionaSegunPosicion (k:ks) xs 
    | k < 0 || k >= m  = Nothing:(seleccionaSegunPosicion ks xs)
    | otherwise = (Just (xs!!k)):(seleccionaSegunPosicion ks xs)
    where m = length xs

-- María Ruiz (2)
-- ==============

cercanoC2 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercanoC2 p n xs | null zs   = Nothing
                 | otherwise = Just (head zs)
    where m  = length xs
          zs = [x | x <- listaDesde xs n, p x]

intercalaC2 (x:xs) (y:ys) = x:y:(intercalaC2 xs ys)
intercalaC2 _ _           = []

listaDesde :: [a] -> Int -> [a]
listaDesde xs k 
    | k >= length xs -1 = reverse xs
    | k < 0             = xs
    | otherwise         = intercalaC2 (drop k xs) (reverse (take k xs))

-- María Ruiz (3)
-- ==============

cercanoC3 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercanoC3 p n xs | null zs   = Nothing
                 | otherwise = Just (head zs)
    where m  = length xs
          zs = [x | x <- listaDesdeC3 xs n, p x]

intercalaC3 :: [a] -> [a] -> [a]
intercalaC3 xs []         = xs
intercalaC3 [] ys         = ys
intercalaC3 (x:xs) (y:ys) = x:y:(intercalaC3 xs ys)

listaDesdeC3 :: [a] -> Int -> [a]
listaDesdeC3 xs k | k >= length xs -1 = reverse xs
                  | k <= 0            = xs
                  | otherwise         = z:intercalaC3 zs (reverse ys)
    where (ys,(z:zs)) = splitAt k xs

-- David
-- =====

cercanoC4 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercanoC4 p n xs = aux p (mezcla ys zs)
 where zs = drop (n+1) xs
       ys = reverse $ take (n+1) xs
       aux _  [] = Nothing
       aux p (x:xs) | p x       = Just x
                    | otherwise = aux p xs

mezcla :: [t] -> [t] -> [t]
mezcla ys [] = ys
mezcla [] zs = zs
mezcla (y:ys) (z:zs) = y:z:mezcla ys zs

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f = 
    cercano (`elem` "aeiou") 6 "Sevilla"     ==  Just 'a' &&
    cercano (`elem` "aeiou") 1 "Sevilla"     ==  Just 'e' &&
    cercano (`elem` "aeiou") 2 "Sevilla"     ==  Just 'i' &&
    cercano (`elem` "aeiou") 5 "Sevilla"     ==  Just 'a' &&
    cercano (`elem` "aeiou") 9 "Sevilla"     ==  Just 'a' &&
    cercano (`elem` "aeiou") (-3) "Sevilla"  ==  Just 'e' &&
    cercano (`elem` "xyz") (-3) "Sevilla"    ==  Nothing
    where cercano = f

verifica2 f =
    zip [1..] ejemplos
    where ejemplos = [cercano (`elem` "aeiou") 6 "Sevilla"     ==  Just 'a', 
                      cercano (`elem` "aeiou") 1 "Sevilla"     ==  Just 'e', 
                      cercano (`elem` "aeiou") 2 "Sevilla"     ==  Just 'i', 
                      cercano (`elem` "aeiou") 5 "Sevilla"     ==  Just 'a', 
                      cercano (`elem` "aeiou") 9 "Sevilla"     ==  Just 'a', 
                      cercano (`elem` "aeiou") (-3) "Sevilla"  ==  Just 'e', 
                      cercano (`elem` "xyz") (-3) "Sevilla"    ==  Nothing]
          cercano = f



-- Referencia
-- ==========

-- El ejercicio está basado en el 
-- [problema del 12 de mayo](http://bit.ly/1mcAZOz)
-- de [1HaskellADay](https://twitter.com/1HaskellADay).
