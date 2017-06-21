-- Primos_equidistantes.hs
-- Primos equidistantes.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 27 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función 
--    primosEquidistantes :: Integer -> [(Integer,Integer)]
-- tal que (primosEquidistantes k) es la lista de los pares de primos
-- cuya diferencia es k. Por ejemplo,
--    take 3 (primosEquidistantes 2)  ==  [(3,5),(5,7),(11,13)]
--    take 3 (primosEquidistantes 4)  ==  [(7,11),(13,17),(19,23)]
--    take 3 (primosEquidistantes 6)  ==  [(23,29),(31,37),(47,53)]
--    take 3 (primosEquidistantes 8)  ==  [(89,97),(359,367),(389,397)]
-- ---------------------------------------------------------------------

primosEquidistantes :: Integer -> [(Integer,Integer)]
primosEquidistantes k = aux primos
    where aux (x:y:ps) | y - x == k = (x,y) : aux (y:ps)
                       | otherwise  = aux (y:ps)

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]
