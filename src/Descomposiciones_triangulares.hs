-- Descomposición_triangular.hs
-- Descomposiciones triangulares.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  4 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
-- tal que (descomposicionesTriangulares n) es la lista de las
-- ternas correspondientes a las descomposiciones de n en tres sumandos,
-- como máximo, formados por números triangulares. Por ejemplo,
--    ghci> descomposicionesTriangulares 6
--    [(0,0,6),(0,3,3)]
--    ghci> descomposicionesTriangulares 26
--    [(1,10,15),(6,10,10)]
--    ghci> descomposicionesTriangulares 96
--    [(3,15,78),(6,45,45),(15,15,66),(15,36,45)]
-- ---------------------------------------------------------------------

import Test.HUnit

descomposicionesTriangulares1 :: Int -> [(Int, Int, Int)] 
descomposicionesTriangulares1 n =         
    [(x,y,n-x-y) | x <- xs, 
                   y <- dropWhile (<x) xs, 
                   n-x-y `elem` dropWhile (<y) xs]
    where xs = takeWhile (<=n) triangulares

-- triangulares es la lista de los números triangulares. Por ejemplo,
--    take 10 triangulares  ==  [0,1,3,6,10,15,21,28,36,45]
triangulares :: [Int]
triangulares = scanl (+) 0 [1..]

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
descomposicionesTriangularesA1 :: Int -> [(Int, Int, Int)]
descomposicionesTriangularesA1 n = 
    [(a,b,n-a-b) | a <- xs, b <- xs, n-a-b `elem` xs, a <= b, b <= n-a-b]
    where xs           = take (n+1) triangulares 
          triangulares = [n*(n+1) `div` 2 | n <- [0..]]

-- Laura
descomposicionesTriangularesA2 :: Int -> [(Int, Int, Int)]
descomposicionesTriangularesA2 x = 
    [(n,m,p)| p <- triangHasta x, m <- triangHasta p, n <- triangHasta m, n+m+p == x]
    where triangHasta x = takeWhile (<=x) [n*(n+1) `div` 2| n <- [0..]]

-- ---------------------------------------------------------------------
-- § Comparaciones                                                    --
-- ---------------------------------------------------------------------

-- Las comparaciones son
--    ghci> length (descomposicionesTriangulares1 500)
--    7
--    (0.02 secs, 518180 bytes)
--    ghci> length (descomposicionesTriangularesA1 500)
--    7
--    (7.88 secs, 25791088 bytes)
-- 
--    ghci> length (descomposicionesTriangulares1 10000)
--    42
--    (0.66 secs, 1545536 bytes)
--    ghci> length (descomposicionesTriangularesA2 10000)
--    42
--    (4.76 secs, 189115136 bytes)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

descomposicionesTriangulares :: Int -> [(Int, Int, Int)]
descomposicionesTriangulares = descomposicionesTriangularesA2

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          descomposicionesTriangulares 6
          ~?= [(0,0,6),(0,3,3)],
          "2" ~: "ej2" ~:
          descomposicionesTriangulares 26
          ~?= [(1,10,15),(6,10,10)],
          "3" ~: "ej3" ~:
          descomposicionesTriangulares 96
          ~?= [(3,15,78),(6,45,45),(15,15,66),(15,36,45)]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}
