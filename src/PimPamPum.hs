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
