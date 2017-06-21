-- Trenza.hs
-- Trenzado de listas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 9 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función 
--    trenza :: [a] -> [a] -> [a]
-- tal que (trenza xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--    trenza [5,1] [2,7,4]             ==  [5,2,1,7]
--    trenza [5,1,7] [2..]             ==  [5,2,1,3,7,4]
--    trenza [2..] [5,1,7]             ==  [2,5,3,1,4,7]
--    take 8 (trenza [2,4..] [1,5..])  ==  [2,1,4,5,6,9,8,13]
-- ---------------------------------------------------------------------

import Test.HUnit

-- 1ª definición (por comprensión):
trenza1 :: [a] -> [a] -> [a]
trenza1 xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- 2ª definición (por zipWith):
trenza2 :: [a] -> [a] -> [a]
trenza2 xs ys = concat (zipWith par xs ys)
    where par x y = [x,y]

-- 3ª definición (por zipWith y sin argumentos):
trenza3 :: [a] -> [a] -> [a]
trenza3 = (concat .) . zipWith par
    where par x y = [x,y]

-- 4ª definición (por recursión):
trenza4 :: [a] -> [a] -> [a]
trenza4 (x:xs) (y:ys) = x : y : trenza xs ys
trenza4 _      _      = []

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
trenzaA1 :: [a] -> [a] -> [a]
trenzaA1 (x:xs) (y:ys) = x:y:trenzaA1 xs ys
trenzaA1 _ _           = []

-- Laura
trenzaA2 :: [a] -> [a] -> [a]
trenzaA2 [] (y:ys) = []
trenzaA2 (x:xs) [] = []
trenzaA2 (x:xs) (y:ys) = x : y : trenzaA2 xs ys

-- David
trenzaA3 :: [a] -> [a] -> [a]
trenzaA3 xs ys = concat [[a,b] | (a,b) <- zip xs ys]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

trenza :: [a] -> [a] -> [a]
trenza = trenzaA3

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~: 
          trenza [5,1] [2,7,4]             ~?=  [5,2,1,7],
          "2" ~: "ej2" ~: 
          trenza [5,1,7] [2..]             ~?=  [5,2,1,3,7,4],
          "3" ~: "ej3" ~: 
          trenza [2..] [5,1,7]             ~?=  [2,5,3,1,4,7],
          "4" ~: "ej4" ~: 
          take 8 (trenza [2,4..] [1,5..])  ~?=  [2,1,4,5,6,9,8,13]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
