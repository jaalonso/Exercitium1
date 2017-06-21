-- Ventana_deslizante.hs
-- Ventana deslizante.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Mayo de 2014
-- ---------------------------------------------------------------------

import Data.List
import Test.HUnit

-- ---------------------------------------------------------------------
-- Definir la función
--    ventanas :: Int -> Int -> [a] -> [[a]]
-- tal que (ventanas x y zs) es la lista de ventanas de zs de tamaño x
-- y deslizamiento y; es decir listas de x elementos consecutivos de zs
-- (salvo, posiblemente, la última que puede ser menor) tales que la
-- diferencia de posiciones entre los primeros elementos de ventanas
-- consecutivas es y. Por ejemplo, 
--    ventanas 3 2 [5,1,9,2] == [[5,1,9],[9,2]]
--    ventanas 3 3 [5,1,9,2] == [[5,1,9],[2]]
--    ventanas 3 4 [5,1,9,2] == [[5,1,9]]
--    ventanas 4 1 [1..7]    == [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]
--    ventanas 4 2 [1..7]    == [[1,2,3,4],[3,4,5,6],[5,6,7]]
--    ventanas 4 3 [1..7]    == [[1,2,3,4],[4,5,6,7]]
--    ventanas 4 4 [1..7]    == [[1,2,3,4],[5,6,7]]
--    ventanas 4 5 [1..7]    == [[1,2,3,4],[6,7]]
--    ventanas 4 6 [1..7]    == [[1,2,3,4],[7]]
--    ventanas 4 7 [1..7]    == [[1,2,3,4]]
--    ventanas 4 8 [1..7]    == [[1,2,3,4]]
--    ventanas 3 2 "abcdef"  == ["abc","cde","ef"]
--    ventanas 3 3 "abcdef"  == ["abc","def"]
--    ventanas 3 4 "abcdef"  == ["abc","ef"]
--    ventanas 3 5 "abcdef"  == ["abc","f"]
--    ventanas 3 6 "abcdef"  == ["abc"]
--    ventanas 3 7 "abcdef"  == ["abc"]
--    ventanas 1 5 "abcdef"  == ["a","f"]
-- ---------------------------------------------------------------------

-- 1º definición (por recursión):
ventanas1 :: Int -> Int -> [a] -> [[a]]
ventanas1 _ _ [] = []
ventanas1 x y zs 
    | length zs <= x = [zs]
    | otherwise      = take x zs : ventanas1 x y (drop y zs)

-- 2ª definición (con unfoldr):
ventanas2 :: Int -> Int -> [a] -> [[a]]
ventanas2 x y = unfoldr aux
  where aux [] = Nothing
        aux xs = Just (ys,zs)
                 where (ys,us)        = splitAt x xs
                       zs | null us   = []
                          | otherwise = drop y xs

-- ---------------------------------------------------------------------
-- § Soluciones propuestas                                            --
-- ---------------------------------------------------------------------

-- María Ruiz
-- ==========

ventanasP1 :: Int -> Int -> [a] -> [[a]]
ventanasP1 _ _ [] = []
ventanasP1 x y zs 
    | length zs <= x = [zs]
    | otherwise      = take x zs : ventanasP1 x y (drop y zs)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

ventanas :: Int -> Int -> [a] -> [[a]]
ventanas = ventanasP1

ejemplos :: Test
ejemplos =
    test ["1" ~: "ventanas 4 1 [1..7]" ~:
          ventanas 4 1 [1..7]   ~?= [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]],
          "2" ~: "ventanas 4 2 [1..7]" ~:
          ventanas 4 2 [1..7]   ~?= [[1,2,3,4],[3,4,5,6],[5,6,7]],
          "3" ~: "ventanas 4 3 [1..7]" ~:
          ventanas 4 3 [1..7]   ~?= [[1,2,3,4],[4,5,6,7]],
          "4" ~: "ventanas 4 4 [1..7]" ~:
          ventanas 4 4 [1..7]   ~?= [[1,2,3,4],[5,6,7]],
          "5" ~: "ventanas 4 5 [1..7]" ~:
          ventanas 4 5 [1..7]   ~?= [[1,2,3,4],[6,7]],
          "6" ~: "ventanas 4 6 [1..7]" ~:
          ventanas 4 6 [1..7]   ~?= [[1,2,3,4],[7]],
          "7" ~: "ventanas 4 7 [1..7]" ~:
          ventanas 4 7 [1..7]   ~?= [[1,2,3,4]],
          "8" ~: "ventanas 4 8 [1..7]" ~:
          ventanas 4 8 [1..7]   ~?= [[1,2,3,4]],
          "9" ~: "ventanas 3 2 \"abcdef\"" ~:
          ventanas 3 2 "abcdef" ~?= ["abc","cde","ef"],
          "10" ~: "ventanas 3 3 \"abcdef\"" ~:
          ventanas 3 3 "abcdef" ~?= ["abc","def"],
          "11" ~: "ventanas 3 4 \"abcdef\"" ~:
          ventanas 3 4 "abcdef" ~?= ["abc","ef"],
          "12" ~: "ventanas 3 5 \"abcdef\"" ~:
          ventanas 3 5 "abcdef" ~?= ["abc","f"],
          "13" ~: "ventanas 3 6 \"abcdef\"" ~:
          ventanas 3 6 "abcdef" ~?= ["abc"],
          "14" ~: "ventanas 3 7 \"abcdef\"" ~:
          ventanas 3 7 "abcdef" ~?= ["abc"],
          "15" ~: "ventanas 1 5 \"abcdef\"" ~:
          ventanas 1 5 "abcdef" ~?= ["a","f"],
          "16" ~: "ventanas 3 2 [5,1,9,2]" ~:
          ventanas 3 2 [5,1,9,2] ~?= [[5,1,9],[9,2]],
          "17" ~: "ventanas 3 3 [5,1,9,2]" ~:
          ventanas 3 3 [5,1,9,2] ~?= [[5,1,9],[2]],  
          "18" ~: "ventanas 3 4 [5,1,9,2]" ~:
          ventanas 3 4 [5,1,9,2] ~?= [[5,1,9]]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- El ejercicio está basado en el 
-- [problema del 30 de abril de 2014](http://bit.ly/1thYgmV) 
-- de [1HaskellADay](https://twitter.com/1HaskellADay).
