-- Separacion_por_posicion.hs
-- Separación por posición.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 2 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    particion :: [a] -> ([a],[a])
-- tal que (particion xs) es el par cuya primera componente son los
-- elementos de xs en posiciones pares y su segunda componente son los
-- restantes elementos. Por ejemplo,
--    particion [3,5,6,2]    ==  ([3,6],[5,2])
--    particion [3,5,6,2,7]  ==  ([3,6,7],[5,2])
--    particion "particion"  ==  ("priin","atco")
-- ---------------------------------------------------------------------

import Test.HUnit

-- 1ª definición (por recursión y auxiliares con recursión cruzada)
-- ================================================================
particion1 :: [a] -> ([a],[a])
particion1 xs = (pares xs, impares xs)

-- (pares xs) es la lista de los elementos de xs en posiciones
-- pares. Por ejemplo,
--    pares [3,5,6,2]  ==  [3,6]
pares :: [a] -> [a]
pares []     = []
pares (x:xs) = x : impares xs

-- (impares xs) es la lista de los elementos de xs en posiciones
-- impares. Por ejemplo,
--    impares [3,5,6,2]  ==  [5,2]
impares :: [a] -> [a]
impares []     = []
impares (x:xs) = pares xs

-- 2ª definición (por comprensión)
-- ===============================
particion2 :: [a] -> ([a],[a])
particion2 xs = ([x | (x,y) <- ps, even y], [x | (x,y) <- ps, odd y])
    where ps = zip xs [0..]

-- 3ª definición (por comprensión e índices)
-- =========================================
particion3 :: [a] -> ([a],[a])
particion3 xs = 
    ([xs!!k | k <- [0,2..n]],[xs!!k | k <- [1,3..n]]) 
    where n = length xs - 1

-- 4ª definición (por recursión)
-- =============================
particion4 :: [a] -> ([a],[a])
particion4 []     = ([],[])
particion4 (x:xs) = (x:zs,ys) where (ys,zs) = particion4 xs  

-- 5ª definición (por plegado)
-- ===========================
particion5 :: [a] -> ([a],[a])
particion5 = foldr f ([],[]) where f x (ys,zs) = (x:zs,ys)

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
-- ====
particionA1 :: [a] -> ([a],[a])
particionA1 xs = (ys,zs)
    where ys = [y | (x,y) <- zip [0..] xs, even x]
          zs = [z | (x,z) <- zip [0..] xs, odd x]

-- Laura
-- =====
particionA2 :: [a] -> ([a],[a])
particionA2 xs = (pares,impares)
    where pares   = [xs!!n | n <- [0,2..length xs-1]]          
          impares = [xs!!m | m <- [1,3..length xs-1]]

-- Miguel
-- ======
particionA3 :: [a] -> ([a],[a])
particionA3 xs = (posicionesPares xs, posicionesImpares xs)

posiciones xs = [(x,y) | (x,y) <- zip xs [0..n]]
    where n = (length xs) -1

posicionesPares xs = [x | (x,y) <- posiciones xs, even y]

posicionesImpares xs = [x | (x,y) <- posiciones xs, odd y]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

particion :: [a] -> ([a],[a])
particion = particionA3

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          particion [3,5,6,2]    ~?=  ([3,6],[5,2]),
          "2" ~: "ej2" ~:
          particion [3,5,6,2,7]  ~?=  ([3,6,7],[5,2]),
          "3" ~: "ej3" ~:
          particion "particion"  ~?=  ("priin","atco")]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}
