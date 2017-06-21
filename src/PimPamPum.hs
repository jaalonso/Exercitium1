-- PimPamPum.hs
-- Pim, Pam, Pum y divisibilidad.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 2 de Septiembre de 2013
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    sonido :: Int -> String
-- tal que (sonido n) escribe "Pim" si n es divisible por 3, además
-- escribe "Pam" si n es divisible por 5 y también escribe "Pum" si n es
-- divisible por 7. Por ejemplo,
--    sonido   3  ==  "Pim"
--    sonido   5  ==  "Pam"
--    sonido   7  ==  "Pum"
--    sonido   8  ==  ""
--    sonido   9  ==  "Pim"
--    sonido  15  ==  "PimPam"
--    sonido  21  ==  "PimPum"
--    sonido  35  ==  "PamPum"
--    sonido 105  ==  "PimPamPum"
-- ---------------------------------------------------------------------

import Test.HUnit

-- 1ª solución
sonido1 :: Int -> String
sonido1 x = concat [z | (x,z) <- zs, x == 0]
    where xs = [rem x 3, rem x 5, rem x 7]
          zs = zip xs ["Pim","Pam","Pum"]  

-- 2ª solución
sonido2 :: Int -> String
sonido2 n = concat (["Pim" | rem n 3 == 0] ++ 
                    ["Pam" | rem n 5 == 0] ++ 
                    ["Pum" | rem n 7 == 0])

-- 3ª solución
sonido3 :: Int -> String
sonido3 n = f 3 "Pim" ++ f 5 "Pam" ++ f 7 "Pum"
    where f x c = if rem n x == 0 then c else "" 

-- 4ª solución
sonido4 :: Int -> String
sonido4 n = 
    concat [s | (s,d) <- zip ["Pim","Pam","Pum"] [3,5,7], rem n d == 0]

-- ---------------------------------------------------------------------
-- § Soluciones de los alumno                                         --
-- ---------------------------------------------------------------------

-- Laura
-- =====

sonidoA1 :: Int -> String
sonidoA1 n = 
    concat [s | (s,d) <- zip ["Pim","Pam","Pum"] [3,5,7], rem n d == 0]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

sonido :: Int -> String
sonido = sonidoA1

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~:
          sonido   3  ~?=  "Pim",
          "2" ~: "ej2" ~:
          sonido   5  ~?=  "Pam",
          "3" ~: "ej3" ~:
          sonido   7  ~?=  "Pum",
          "4" ~: "ej4" ~:
          sonido   8  ~?=  "",
          "5" ~: "ej5" ~:
          sonido   9  ~?=  "Pim",
          "6" ~: "ej6" ~:
          sonido  15  ~?=  "PimPam",
          "7" ~: "ej7" ~:
          sonido  21  ~?=  "PimPum",
          "8" ~: "ej8" ~:
          sonido  35  ~?=  "PamPum",
          "9" ~: "ej9" ~:
          sonido 105  ~?=  "PimPamPum"]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica
--    Cases: 18  Tried: 18  Errors: 0  Failures: 0
--    Counts {cases = 18, tried = 18, errors = 0, failures = 0}
