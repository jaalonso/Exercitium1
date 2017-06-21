-- Sucesiones_pucelanas.hs
-- Sucesiones pucelanas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 12 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- En la Olimpiada de Matemática del 2010 se planteó el siguiente
-- problema:  
--   Una sucesión pucelana es una sucesión creciente de 16 números
--   impares positivos consecutivos, cuya suma es un cubo perfecto. 
--   ¿Cuántas sucesiones pucelanas tienen solamente números de tres 
--   cifras? 
-- Para resolverlo se propone el siguiente ejercicio.

-- ---------------------------------------------------------------------
-- Definir la función
--    pucelanasConNcifras :: Int -> [[Int]]
-- tal que (pucelanasConNcifras n) es la lista de las sucesiones
-- pucelanas que tienen solamente números de n cifras. Por ejemplo,
--    ghci> pucelanasConNcifras 2
--    [[17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47]]
-- Calcular cuántas sucesiones pucelanas tienen solamente números de
-- tres cifras. 
-- ---------------------------------------------------------------------

pucelanasConNcifras :: Int -> [[Int]]
pucelanasConNcifras n = 
    [[x,x+2..x+30] | x <- [10^(n-1)+1..10^n-31],
                     esCubo (sum [x,x+2..x+30])]

-- (esCubo n) se verifica si n es un cubo. Por ejemplo,
--    esCubo 27  ==  True
--    esCubo 28  ==  False
esCubo x = y^3 == x
    where y = ceiling (fromIntegral x ** (1/3))

-- El cálculo es
--    ghci> length (pucelanasConNcifras 3)
--    3

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Eduardo
-- =======

pucelanasConNcifrasA1 :: Int -> [[Int]]
pucelanasConNcifrasA1 n = takeWhile (f n) (dropWhile (f (n-1))  zs)
 where zs = [ys|x<-pucelanas,ys<- [[(x+1),(x+3)..(x+32)]]]
       f n ys  = and [length (show y) <= n|y<-ys] 

pucelanas :: [Int]
pucelanas = [((div (n^3) 256)-1)*16 |n<-[1..],mod (n^3) 256 == 0]

-- David
-- =====

pucelanasConNcifrasA2 :: Int -> [[Int]]
pucelanasConNcifrasA2 n = takeWhile (f n) (dropWhile (f (n-1)) zs)
    where zs = [ys | (x,n) <- pucelanasA2, 
                     ys <- [[(x+1),(x+3)..(x+31)]],
                     sum ys == n^3]
          f n ys = and [length (show y) <= n|y<-ys] 

pucelanasA2 :: [(Int, Int)]
pucelanasA2 = [(g n + f n,n) | n <- [1..], n^3 >= 512]
    where g n = ((div (n^3) 256)-1)*16
          f n | mod (n^3) 256 == 0 = 0
              | otherwise = div 256 $  mod (n^3) 256

cuantsSucesnsPucelns3cfrs :: Int
cuantsSucesnsPucelns3cfrs = length $ pucelanasConNcifras 3
