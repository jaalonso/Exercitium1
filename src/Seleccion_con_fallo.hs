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

-- ---------------------------------------------------------------------
-- Soluciones de alumnos
-- ---------------------------------------------------------------------

-- Luis
-- =====
seleccionConFalloA1 :: (a -> Bool) -> [a] -> [a]
seleccionConFalloA1 p xs 
    | length (fst (span p xs)) < length xs = (fst (span p xs))++[head (snd(span p xs))]
    | otherwise                            = fst (span p xs)

-- Luis Portillo
-- =============
seleccionConFalloA2 :: (a -> Bool) -> [a] -> [a]
seleccionConFalloA2 p (x:xs) | p x = x : seleccionConFallo p xs
                             | otherwise = [x]
seleccionConFalloA2 _ _ = []

-- Simón
-- =====
seleccionConFalloA3 p [] = []
seleccionConFalloA3 p (x:xs) 
    | verificaP p x = x: seleccionConFalloA3 p xs
    | otherwise     = [x]

verificaP p x | p x       = True
              | otherwise = False

-- Daniel
-- ======
seleccionConFalloA4 p []=[]
seleccionConFalloA4 p (x:xs)| p x = x: seleccionConFalloA4 p xs
                            | otherwise = [x]

-- Loles
-- =====
seleccionConFalloA5 :: (a -> Bool) -> [a] -> [a]
seleccionConFalloA5 p xs = take l xs
    where l = (length (takeWhile p xs))+1

-- Victoria
-- ========
seleccionConFalloA6 :: (a -> Bool) -> [a] -> [a]
seleccionConFalloA6 p [] = []
seleccionConFalloA6 p (x:xs) | p x       = x :seleccionConFalloA6 p xs
                             | otherwise = [x]

-- Agustín
-- =======
seleccionConFalloA7 :: (a -> Bool) -> [a] -> [a]
seleccionConFalloA7 p xs =
    if all p xs then xs
    else (fst (span p xs)) ++ [head (snd (span p xs))]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

seleccionConFallo :: (a -> Bool) -> [a] -> [a]
seleccionConFallo = seleccionConFalloA7

ejemplos :: Test
ejemplos =
    test ["1" ~: "seleccionConFallo (<5) [3,2,5,7,1,0]" ~:
          seleccionConFallo (<5) [3,2,5,7,1,0]  ~?=  [3,2,5],
          "2" ~: "seleccionConFallo odd [1..4]        " ~:
          seleccionConFallo odd [1..4]          ~?=  [1,2],
          "3" ~: "seleccionConFallo odd [1,3,5]       " ~:
          seleccionConFallo odd [1,3,5]         ~?=  [1,3,5],
          "4" ~: "seleccionConFallo (<5) [10..20]     " ~:
          seleccionConFallo (<5) [10..20]       ~?=  [10]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
