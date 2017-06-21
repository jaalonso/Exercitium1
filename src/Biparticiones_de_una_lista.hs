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
