-- seleccion_con_fallo.hs
-- Selección hasta el primero que falla inclusive.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    seleccionConFallo :: (a -> Bool) -> [a] -> [a]
-- tal que (seleccionConFallo p xs) es la lista de los elementos de xs
-- que cumplen el predicado p hasta el primero que no lo cumple
-- inclusive. Por ejemplo,
--    seleccionConFallo (<5) [3,2,5,7,1,0]  ==  [3,2,5]
--    seleccionConFallo odd [1..4]          ==  [1,2]
--    seleccionConFallo odd [1,3,5]         ==  [1,3,5]
--    seleccionConFallo (<5) [10..20]       ==  [10]
-- ---------------------------------------------------------------------

import Test.HUnit

-- 1ª solución (por recursión):
seleccionConFallo1 :: (a -> Bool) -> [a] -> [a]
seleccionConFallo1 p []                 = []
seleccionConFallo1 p (x:xs) | p x       = x : seleccionConFallo1 p xs
                            | otherwise = [x]

-- 2ª solución (con span):
seleccionConFallo2 :: (a -> Bool) -> [a] -> [a]
seleccionConFallo2 p xs = ys ++ take 1 zs
    where (ys,zs) = span p xs

-- ---------------------------------------------------------------------
-- § Soluciones avanzadas                                             --
-- ---------------------------------------------------------------------

-- 1ª solución (sin argumentos):
seleccionConFalloAv1 :: (a -> Bool) -> [a] -> [a]
seleccionConFalloAv1 = ((uncurry (++) . fmap (take 1)) .) . span

-- Ejemplo de cálculo:
--    seleccionConFallo (<5) [3,2,5,7,1,0]
--    = (((uncurry (++) . fmap (take 1)) .) . span) (<5) [3,2,5,7,1,0]
--    = (uncurry (++) . fmap (take 1)) ([3,2],[5,7,1,0])
--    = uncurry (++) ([3,2],[5])
--    = [3,2,5]

-- Nota: (fmap f (x,y)) es equivalente a (x,f y).
