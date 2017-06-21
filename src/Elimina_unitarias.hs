-- Elimina_unitarias.hs
-- Eliminación de las ocurrencias unitarias.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 8 de Junio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Ejercicio                                                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    eliminaUnitarias :: Char -> String -> String
-- tal que (eliminaUnitarias c cs) es la lista obtenida eliminando de la
-- cadena cs las ocurrencias unitarias del carácter c (es decir,
-- aquellas ocurrencias de c tales que su elemento anterior y posterior
-- es distinto de c). Por ejemplo,
--    eliminaUnitarias 'X ""                  == ""
--    eliminaUnitarias 'X "X"                 == ""
--    eliminaUnitarias 'X "XX"                == "XX"
--    eliminaUnitarias 'X "XXX"               == "XXX"
--    eliminaUnitarias 'X "abcd"              == "abcd"
--    eliminaUnitarias 'X "Xabcd"             == "abcd"
--    eliminaUnitarias 'X "XXabcd"            == "XXabcd"
--    eliminaUnitarias 'X "XXXabcd"           == "XXXabcd"
--    eliminaUnitarias 'X "abcdX"             == "abcd"
--    eliminaUnitarias 'X "abcdXX"            == "abcdXX"
--    eliminaUnitarias 'X "abcdXXX"           == "abcdXXX"
--    eliminaUnitarias 'X "abXcd"             == "abcd"
--    eliminaUnitarias 'X "abXXcd"            == "abXXcd"
--    eliminaUnitarias 'X "abXXXcd"           == "abXXXcd"
--    eliminaUnitarias 'X "XabXcdX"           == "abcd"
--    eliminaUnitarias 'X "XXabXXcdXX"        == "XXabXXcdXX"
--    eliminaUnitarias 'X "XXXabXXXcdXXX"     == "XXXabXXXcdXXX"
--    eliminaUnitarias 'X' "XabXXcdXeXXXfXx"  ==  "abXXcdeXXXfx"
-- ---------------------------------------------------------------------

import Data.List (group)
import Data.Array
import Test.HUnit

-- 1ª solución (por comprensión):
eliminaUnitarias1 :: Char -> String -> String
eliminaUnitarias1 c cs = concat [xs | xs <- group cs, xs /= [c]] 

-- 2ª solución (por composición):
eliminaUnitarias2 :: Char -> String -> String
eliminaUnitarias2 c = concat . filter (/=[c]) . group

-- 3ª solución (por recursión):
eliminaUnitarias3 :: Char -> String -> String
eliminaUnitarias3 _ [] = []
eliminaUnitarias3 c [x] | c == x    = []
                        | otherwise = [x]
eliminaUnitarias3 c (x:y:zs) 
    | x /= c    = x : eliminaUnitarias3 c (y:zs)
    | y /= c    = y : eliminaUnitarias3 c zs
    | otherwise = takeWhile (==c) (x:y:zs) ++ 
                  eliminaUnitarias3 c (dropWhile (==c) zs)

-- 4ª solución (por recursión con acumuladores):
eliminaUnitarias4 :: Char -> String -> String
eliminaUnitarias4 c cs = reverse (aux0 cs "")
    where aux0 [] cs2                  = cs2
          aux0 (x:cs1) cs2 | x == c    = aux1 cs1 cs2
                           | otherwise = aux0 cs1 (x:cs2)
          aux1 [] cs2                  = cs2
          aux1 (x:cs1) cs2 | x == c    = aux2 cs1 (c:c:cs2)
                           | otherwise = aux0 cs1 (x:cs2)
          aux2 [] cs2                  = cs2
          aux2 (x:cs1) cs2 | x == c    = aux2 cs1 (c:cs2)
                           | otherwise = aux0 cs1 (x:cs2)

-- 5ª solución (con índices)
eliminaUnitarias5 :: Char -> String -> String
eliminaUnitarias5 c cs = 
    [x | i <- [0..length cs - 1],
               let x = cs!!i, 
               x /= c || ds!!i == c || ds!!(i+2) == c]
    where d  = if c == 'a' then 'b' else 'a'
          ds = d : cs ++ [d]

-- 6ª solución (por recursión con span)
eliminaUnitarias6 :: Char -> String -> String
eliminaUnitarias6 c [] = []
eliminaUnitarias6 c cs | ys == [c] = xs ++ eliminaUnitarias6 c zs
                       | otherwise = xs ++ ys ++ eliminaUnitarias6 c zs
    where (xs,us) = span (/=c) cs
          (ys,zs) = span (==c) us

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Luis
-- ====
eliminaUnitariasA1 :: Char -> String -> String
eliminaUnitariasA1 c cs = concat [ys | ys <- group cs, ys /= [c]]

-- David
-- =====
eliminaUnitariasA2 :: Char -> String -> String
eliminaUnitariasA2 a xs =  [v!j | j <- [1..m], cuales j (v!j)]
    where v = listArray (1,m) xs
          m = length xs
          unitaria y v = and [v!j /= a | j <- [max 1 (y-1).. min m (y+1)], j/=y] 
          cuales y b | b == a = not (unitaria y v)
                     | otherwise = True

-- ---------------------------------------------------------------------
-- § Comparaciones                                                    --
-- ---------------------------------------------------------------------

-- Las comparaciones son
--    ghci> let cs = concat (replicate 10000 ("ax")) ++ "x"
--    ghci> :set +s
--    ghci> last (eliminaUnitarias1 'x' cs)
--    'x'
--    (0.08 secs, 5623064 bytes)
--    ghci> last (eliminaUnitarias2 'x' cs)
--    'x'
--    (0.04 secs, 3106220 bytes)
--    ghci> last (eliminaUnitarias3 'x' cs)
--    'x'
--    (0.05 secs, 1550660 bytes)
--    ghci> last (eliminaUnitarias4 'x' cs)
--    'x'
--    (0.05 secs, 2096632 bytes)
--    ghci> last (eliminaUnitarias5 'x' cs)
--    'x'
--    (4.42 secs, 5686592 bytes)
--    ghci> let cs = concat (replicate 20000 ("ax")) ++ "x"
--    (0.00 secs, 0 bytes)
--    ghci> last (eliminaUnitarias1 'x' cs)
--    'x'
--    (0.16 secs, 10730068 bytes)
--    ghci> last (eliminaUnitarias2 'x' cs)
--    'x'
--    (0.07 secs, 6724396 bytes)
--    ghci> last (eliminaUnitarias3 'x' cs)
--    'x'
--    (0.08 secs, 3094052 bytes)
--    ghci> last (eliminaUnitarias4 'x' cs)
--    'x'
--    (0.10 secs, 4156652 bytes)
--    ghci> last (eliminaUnitarias5 'x' cs)
--    'x'
--    (16.32 secs, 10857088 bytes)
--    ghci> last (eliminaUnitarias6 'x' cs)
--    'x'
--    (0.06 secs, 5694820 bytes)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

eliminaUnitarias :: Char -> String -> String
eliminaUnitarias = eliminaUnitariasA2

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          eliminaUnitarias 'X' ""                  ~?= "",
          "2" ~: "ej2" ~:
          eliminaUnitarias 'X' "X"                 ~?= "",
          "3" ~: "ej3" ~:
          eliminaUnitarias 'X' "XX"                ~?= "XX",
          "4" ~: "ej4" ~:
          eliminaUnitarias 'X' "XXX"               ~?= "XXX",
          "5" ~: "ej5" ~:
          eliminaUnitarias 'X' "abcd"              ~?= "abcd",
          "6" ~: "ej6" ~:
          eliminaUnitarias 'X' "Xabcd"             ~?= "abcd",
          "7" ~: "ej7" ~:
          eliminaUnitarias 'X' "XXabcd"            ~?= "XXabcd",
          "8" ~: "ej8" ~:
          eliminaUnitarias 'X' "XXXabcd"           ~?= "XXXabcd",
          "9" ~: "ej9" ~:
          eliminaUnitarias 'X' "abcdX"             ~?= "abcd",
          "10" ~: "ej10" ~:
          eliminaUnitarias 'X' "abcdXX"            ~?= "abcdXX",
          "11" ~: "ej11" ~:
          eliminaUnitarias 'X' "abcdXXX"           ~?= "abcdXXX",
          "12" ~: "ej12" ~:
          eliminaUnitarias 'X' "abXcd"             ~?= "abcd",
          "13" ~: "ej13" ~:
          eliminaUnitarias 'X' "abXXcd"            ~?= "abXXcd",
          "14" ~: "ej14" ~:
          eliminaUnitarias 'X' "abXXXcd"           ~?= "abXXXcd",
          "15" ~: "ej15" ~:
          eliminaUnitarias 'X' "XabXcdX"           ~?= "abcd",
          "16" ~: "ej16" ~:
          eliminaUnitarias 'X' "XXabXXcdXX"        ~?= "XXabXXcdXX",
          "17" ~: "ej17" ~:
          eliminaUnitarias 'X' "XXXabXXXcdXXX"     ~?= "XXXabXXXcdXXX",
          "18" ~: "ej18" ~:
          eliminaUnitarias 'X' "XabXXcdXeXXXfXx"   ~?=  "abXXcdeXXXfx"]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el ejercicio [Remove singleton](http://bit.ly/1pqfxoV)
-- publicado el 6 de junio de 2014 en 
-- [Programming Praxis](http://programmingpraxis.com). 

