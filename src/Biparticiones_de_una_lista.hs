-- Biparticiones_de_una_lista.hs
-- Biparticiones de una lista.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 19 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    biparticiones :: [a] -> [([a],[a])]
-- tal que (biparticiones xs) es la lista de pares formados por un
-- prefijo de xs y el resto de xs. Por ejemplo,
--    ghci> biparticiones [3,2,5]
--    [([],[3,2,5]),([3],[2,5]),([3,2],[5]),([3,2,5],[])]
--    ghci> biparticiones "Roma"
--    [("","Roma"),("R","oma"),("Ro","ma"),("Rom","a"),("Roma","")]
-- ---------------------------------------------------------------------

import Data.List
import Control.Applicative
import Test.HUnit

-- 1ª definición (con splitAt):
biparticiones1 :: [a] -> [([a],[a])]
biparticiones1 xs = [splitAt i xs | i <- [0..length xs]]

-- 2ª definición (con inits y tails):
biparticiones2 :: [a] -> [([a],[a])]
biparticiones2 xs = zip (inits xs) (tails xs)

-- 3ª definición (por recursión):
biparticiones3 :: [a] -> [([a],[a])]
biparticiones3 [] = [([],[])] 
biparticiones3 (x:xs) = 
    ([],(x:xs)) : [(x:ys,zs) | (ys,zs) <- biparticiones3 xs]

-- ---------------------------------------------------------------------
-- § Soluciones avanzadas                                             --
-- ---------------------------------------------------------------------

-- 1ª definición (sin argumentos):
biparticionesA1 :: [a] -> [([a],[a])]
biparticionesA1 = liftA2 zip inits tails

-- 2ª definición (sin argumentos):
biparticionesA2 :: [a] -> [([a],[a])]
biparticionesA2 = zip <$> inits <*> tails

-- ---------------------------------------------------------------------
-- § Soluciones propuestas por los alumnos                            --
-- ---------------------------------------------------------------------

-- Laura
biparticionesP1 :: [a] -> [([a],[a])]
biparticionesP1 xs = [(take n xs , drop n xs)| n <- [0..length xs]]

-- Luis
biparticionesP2 :: [a] -> [([a],[a])]
biparticionesP2 xs = zip (reverse (particiones1 xs)) (particiones2 xs)
    where particiones1 []     = [[]]
          particiones1 xs     = xs:particiones1 (init xs)
          particiones2 []     = [[]]
          particiones2 (x:xs) = (x:xs):particiones2 xs

-- David
biparticionesP3 :: [a] -> [([a],[a])]
biparticionesP3 xs = [splitAt n xs | n <- [0..length xs]]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

biparticiones :: [a] -> [([a],[a])]
biparticiones = biparticionesP3

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~: 
          biparticiones [3,2,5] ~?=
          [([],[3,2,5]),([3],[2,5]),([3,2],[5]),([3,2,5],[])],
          "2" ~: "ej2" ~:
          biparticiones "Roma" ~?=
          [("","Roma"),("R","oma"),("Ro","ma"),("Rom","a"),("Roma","")]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
