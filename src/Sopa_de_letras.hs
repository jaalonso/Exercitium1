-- Sopa_de_letras.hs
-- Sopa de letras
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 26 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices se puede representar mediante tablas cuyos índices son
-- pares de números naturales:  
--    type Matriz a = Array (Int,Int) a
-- 
-- Definir la función 
--    enLaSopa :: Eq a => [a] -> Matriz a -> Bool
-- tal que (enLaSopa c p) se verifica si c está en la matriz p en
-- horizontal o en vertical. Por ejemplo, si p es la matriz siguiente:
--    p :: Matriz Char
--    p = listaMatriz ["mjtholueq",
--                     "juhoolauh",
--                     "dariouhyj",
--                     "rngkploaa"]
-- entonces,
--    enLaSopa "dar"  p  ==  True   -- En horizontal a la derecha en la 3ª fila
--    enLaSopa "oir"  p  ==  True   -- En horizontal a la izquierda en la 3ª fila
--    enLaSopa "juan" p  ==  True   -- En vertical descendente en la 2ª columna
--    enLaSopa "kio"  p  ==  True   -- En vertical ascendente en la 3ª columna
--    enLaSopa "Juan" p  ==  False
--    enLaSopa "hola" p  ==  False
-- 
-- Nota. Para resolverlo, se puede usar la función isInfixOf.
-- ---------------------------------------------------------------------

-- Auxiliares
-- ==========

import Data.Array
import Data.List

type Matriz a = Array (Int,Int) a

p :: Matriz Char
p = listaMatriz ["mjtholueq",
                 "juhoolauh",
                 "dariouhyj",
                 "rngkploaa"]

-- 1ª solución
-- ===========

enLaSopa1 :: Eq a => [a] -> Matriz a -> Bool
enLaSopa1 c p = 
    or [isInfixOf c xs | 
        xs <- [[p!(i,j) | j <- [1..n]]     | i <- [1..m]] ++
              [[p!(i,j) | j <- [n,n-1..1]] | i <- [1..m]] ++
              [[p!(i,j) | i <- [1..m]]     | j <- [1..n]] ++
              [[p!(i,j) | i <- [m,m-1..1]] | j <- [1..n]]]
    where (_,(m,n)) = bounds p

-- 2ª solución
-- ===========

enLaSopa2 :: Eq a => [a] -> Matriz a -> Bool
enLaSopa2 c p = estaEnHorizontal c p || estaEnVertical c p 

estaEnHorizontal :: Eq a => [a] -> Matriz a -> Bool
estaEnHorizontal c p =
    or [isInfixOf c xs | xs <- filasL p ++ map reverse (filasL p)]

filasL :: Matriz a -> [[a]]
filasL p = [filaMat i p | i <-[1..numFilas p]]

estaEnVertical :: Eq a => [a] -> Matriz a -> Bool
estaEnVertical c p =
    or [isInfixOf c xs | xs <- columnasL p ++ map reverse (columnasL p)]

columnasL :: Matriz a -> [[a]]
columnasL p = [columnaMat j p | j <- [1..numColumnas p]]

listaMatriz :: [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
    where m = length xss
          n = length (head xss)

numFilas :: Matriz a -> Int
numFilas = fst . snd . bounds

numColumnas:: Matriz a -> Int
numColumnas = snd . snd . bounds

filaMat :: Int -> Matriz a -> [a]
filaMat i p = [p!(i,j) | j <- [1..n]]
    where n = numColumnas p

columnaMat :: Int -> Matriz a -> [a]
columnaMat j p = [p!(i,j) | i <- [1..m]]
    where m = numFilas p

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- David
-- =====

-- import Data.Array
-- import Data.List
--  
-- type Matriz a = Array (Int,Int) a
--  
-- p :: Matriz Char
-- p = listaMatriz ["mjtholueq",
--                  "juhoolauh",
--                  "dariouhyj",
--                  "rngkploaa"]
 
enLaSopaA1 :: Eq a => [a] -> Matriz a -> Bool
enLaSopaA1 c p = or ([isInfixOf c ys | ys <- filas ++ columnas]++
                     [isInfixOf (reverse c) ys | ys <- filas ++ columnas]) 
 where (m,n)    = snd (bounds p)
       filas    = [f i | i <- [1..m]] where f i = [p!(i,j) | j <- [1..n]]
       columnas = [f j | j <- [1..n]] where f j = [p!(i,j) | i <- [1..m]]

-- listaMatriz :: [[a]] -> Matriz a
-- listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
--  where m = length xss
--        n = length (head xss)
 
-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    enLaSopa "dar"  p  ==  True  &&
    enLaSopa "oir"  p  ==  True  &&
    enLaSopa "juan" p  ==  True  &&
    enLaSopa "kio"  p  ==  True  &&
    enLaSopa "Juan" p  ==  False && 
    enLaSopa "hola" p  ==  False
    where enLaSopa = f


