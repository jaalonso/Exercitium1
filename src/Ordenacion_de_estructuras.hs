-- Ordenacion_de_estructuras.hs
-- Ordenación de estructuras
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 11 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las notas de los dos primeros exámenes se pueden representar mediante
-- el siguiente tipo de dato
--    data Notas = Notas String Int Int
--                 deriving (Read, Show, Eq)
-- Por ejemplo, (Notas "Juan" 6 5) representar las notas de un alumno
-- cuyo nombre es Juan, la nota del primer examen es 6 y la del segundo
-- es 5.
-- 
-- Definir la función
--    ordenadas :: [Notas] -> [Notas]
-- tal que (ordenadas ns) es la lista de las notas ns ordenadas
-- considerando primero la nota del examen 2, a continuación la del
-- examen 1 y finalmente el nombre. Por ejemplo,
--    ghci> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7] 
--    [Notas "Juan" 6 5,Notas "Luis" 3 7]
--    ghci> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4] 
--    [Notas "Luis" 3 4,Notas "Juan" 6 5]
--    ghci> ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4] 
--    [Notas "Luis" 7 4,Notas "Juan" 6 5]
--    ghci> ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4] 
--    [Notas "Juan" 6 4,Notas "Luis" 7 4]
--    ghci> ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4] 
--    [Notas "Luis" 5 4,Notas "Juan" 6 4]
--    ghci> ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4] 
--    [Notas "Juan" 5 4,Notas "Luis" 5 4]
--    ghci> ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4] 
--    [Notas "Eva" 5 4,Notas "Juan" 5 4]
-- ---------------------------------------------------------------------

import Data.List (sort)

data Notas = Notas String Int Int
             deriving (Read, Show, Eq)

ordenadas :: [Notas] -> [Notas]
ordenadas ns =
    [Notas n x y | (y,x,n) <- sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns]] 

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Eduardo
ordenadasA1 :: [Notas] -> [Notas]
ordenadasA1 xs = aux (sort [(y,x,xs) | (Notas xs x y) <- xs])
    where aux []           = []
          aux ((a,b,c):xs) = [Notas c b a] ++ aux xs

-- Donde aux traslada tripletes a nuestro tipo de dato.

-- David
ordenadasA2 :: [Notas] -> [Notas]
ordenadasA2 ns = [Notas a b c | (c,b,a) <- xs]
    where xs = sort [(c,b,a) | (Notas a b c) <- ns] 
