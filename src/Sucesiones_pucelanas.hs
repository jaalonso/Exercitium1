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
