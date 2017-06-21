-- Emparejamiento_binario.hs
-- Emparejamiento binario.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 18 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función
--    zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
-- tal que (zipBinario fs xs ys) es la lista obtenida aplicando cada una
-- de las operaciones binarias de fs a los correspondientes elementos de
-- xs e ys. Por ejemplo, 
--    zipBinario [(+), (*), (*)] [2,2,2] [4,4,4]     ==  [6,8,8]
--    zipBinario [(+)] [2,2,2] [4,4,4]               ==  [6]
--    zipBinario (cycle [(+), (*)]) [1 .. 4] [2..5]  ==  [3,6,7,20]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
zipBinario :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario (f:fs) (x:xs) (y:ys) = f x y : zipBinario fs xs ys
zipBinario _ _ _                = []

-- 2ª definición (con zip):
zipBinario2 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario2 fs xs ys = [f x y | (f,(x,y)) <- zip fs (zip xs ys)]

-- 3ª definición (con zip3):
zipBinario3 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario3 fs xs ys = [f x y | (f,x,y) <- zip3 fs xs ys]

-- Nota. La definición anterior usa la función zip3 que agrupa
-- ternas. Por ejemplo,
--    zip3 [1..3] [4..7] [8..11]  ==  [(1,4,8),(2,5,9),(3,6,10)]
--    zip3 [1..3] [4,5] [8..11]   ==  [(1,4,8),(2,5,9)]
--    zip3 [1..3] [4,5] [8]       ==  [(1,4,8)]

-- 4ª definición (con zipWith3):
zipBinario4 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipBinario4 = zipWith3 id 

-- Nota. En la definición anterior se usa la función zipwith3 tal que
-- (zipwith3 g fs xs ys) es la lista obtenida aplicando 
--    zipwith3 (\x y z -> x+y+z) [7,8] [1..3] [4..6]  ==  [12,15]
--    zipwith3 id [(+),(*)] [1..3] [4..6]             ==  [5,10]

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
zipbinarioa1 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipbinarioa1 (f:fs) (x:xs) (y:ys) = [f x y] ++ (zipbinarioa1 fs xs ys)
zipbinarioa1 _ _ _                = []

zipbinarioa2 :: [a -> b -> c] -> [a] -> [b] -> [c]
zipbinarioa2 (f:fs) (x:xs) (y:ys) = (f x y) : (zipbinarioa2 fs xs ys)
zipbinarioa2 _ _ _                = []

-- Ana Vidal
zipBinarioA3 :: [a->b->c]->[a]->[b]->[c]
zipBinarioA3 fs xs ys = [f x y | (f,(x,y)) <- zip fs [(x,y)| (x,y)<- zip xs ys]]

