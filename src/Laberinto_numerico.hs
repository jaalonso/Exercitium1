-- Laberinto_numerico.hs
-- Laberinto numérico.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema del laberinto numérico consiste en, dados un par de
-- números, encontrar la longitud del camino más corto entre ellos
-- usando sólo las siguientes operaciones:  
--    * multiplicar por 2,
--    * dividir por 2 (sólo para los pares) y
--    * sumar 2.
-- Por ejemplo, un camino mínimo 
--    * de  3 a 12 es [3,6,12], 
--    * de 12 a  3 es [12,6,3], 
--    * de  9 a  2 es [9,18,20,10,12,6,8,4,2] y 
--    * de  2 a  9 es [2,4,8,16,18,9].
-- 
-- Definir la función
--    longitudCaminoMinimo :: Int -> Int -> Int
-- tal que (longitudCaminoMinimo x y) es la longitud del camino mínimo
-- desde x hasta y en el laberinto numérico. 
--    longitudCaminoMinimo 3 12  ==  2
--    longitudCaminoMinimo 12 3  ==  2
--    longitudCaminoMinimo 9 2   ==  8
--    longitudCaminoMinimo 2 9   ==  5
-- ---------------------------------------------------------------------

longitudCaminoMinimo :: Int -> Int -> Int
longitudCaminoMinimo x y = 
    head [n | n <- [1..], y `elem` orbita n [x]] 

-- (orbita n xs) es el conjunto de números que se pueden obtener aplicando 
-- como máximo n veces las operaciones a los elementos de xs. Por ejemplo, 
--    orbita 0 [12]  ==  [12]
--    orbita 1 [12]  ==  [6,12,14,24]
--    orbita 2 [12]  ==  [3,6,7,8,12,14,16,24,26,28,48]
orbita :: Int -> [Int] -> [Int]
orbita 0 xs = sort xs
orbita n xs = sort (nub (ys ++ concat [sucesores x | x <- ys]))
    where ys = orbita (n-1) xs
          sucesores x | odd x     = [2*x, x+2]
                      | otherwise = [2*x, x `div` 2, x+2]

