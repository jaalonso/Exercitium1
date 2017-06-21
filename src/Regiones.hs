-- Regiones.hs
-- Regiones en el plano.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 16 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En los siguientes dibujos se observa que el número máximo de regiones
-- en el plano generadas con 1, 2 ó 3 líneas son 2, 4 ó 7,
-- respectivamente. 
-- 
--                       \  |
--                        \5|
--                         \|
--                          \
--                          |\
--                          | \
--                |         |  \ 
--     1        1 | 3     1 | 3 \  6
--    ------   ---|---   ---|----\---
--     2        2 | 4     2 | 4   \ 7
--                |         |      \
-- 
-- Definir la función
--    regiones :: Integer -> Integer  
-- tal que (regiones n) es el número máximo de regiones en el plano
-- generadas con n líneas. Por ejemplo,
--    regiones 3    ==  7  
--    regiones 100  ==  5051
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Test.QuickCheck.Modifiers

-- 1ª definición (por recursión):
regiones1 :: Integer -> Integer  
regiones1 0 = 1
regiones1 n = regiones1 (n-1) + n  

-- 2ª definición (por la fórmula):
regiones2 :: Integer -> Integer
regiones2 n = n*(n+1) `div` 2 + 1

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis F.
regionesA1 :: Integer -> Integer
regionesA1 n = n*(n+1) `div` 2 + 1

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

prop_equivalencia :: Positive Integer -> Bool
prop_equivalencia (Positive n) =
    regiones n == regionesA1 n
