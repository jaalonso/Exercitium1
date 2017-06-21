-- Valor_de_un_polinomio.hs
-- Valores de polinomios representados con vectores.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 7 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Los polinomios se pueden representar mediante vectores usando la
-- librería Data.Array. En primer lugar, se define el tipo de los
-- polinomios (con coeficientes de tipo a) mediante
--    type Polinomio a = Array Int a
-- Como ejemplos, definimos el polinomio
--    ej_pol1 :: Array Int Int
--    ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]
-- que representa a 2x - 5x^2 + 7x^4 + 6 y el polinomio
--    ej_pol2 :: Array Int Double
--    ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]
-- que representa a 2x - 5.2x^2 + 7x^4 + 6.5
--
-- Definir la función
--    valor :: Num a => Polinomio a -> a -> a
-- tal que (valor p b) es el valor del polinomio p en el punto b. Por
-- ejemplo, 
--    valor ej_pol1 0  ==  6
--    valor ej_pol1 1  ==  10
--    valor ej_pol1 2  ==  102
--    valor ej_pol2 0  ==  6.5
--    valor ej_pol2 1  ==  10.3
--    valor ej_pol2 3  ==  532.7
-- ---------------------------------------------------------------------

import Data.Array
import Test.HUnit

type Polinomio a = Array Int a

-- 2x - 5x^2 + 7x^4 + 6
ej_pol1 :: Array Int Int
ej_pol1 = array (0,4) [(1,2),(2,-5),(4,7),(0,6),(3,0)]

-- 2x - 5.2x^2 + 7x^4 + 6.5
ej_pol2 :: Array Int Double
ej_pol2 = array (0,4) [(1,2),(2,-5.2),(4,7),(0,6.5),(3,0)]

-- 1ª definición
valor1 :: Num a => Polinomio a -> a -> a
valor1 p b = sum [(p!i)*b^i | i <- [0..n]]
    where (_,n) = bounds p

-- 2ª definición
valor2 :: Num a => Polinomio a -> a -> a
valor2 p b = sum [v*b^i | (i,v) <- assocs p]

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Laura
valorA1 :: Num a => Polinomio a -> a -> a
valorA1 p b = sum [ y*b^x | (x,y) <- assocs p]

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

valor :: Num a => Polinomio a -> a -> a
valor = valorA1

ejemplos :: Test
ejemplos =
    test ["1" ~: "valor ej1" ~: 
          valor ej_pol1 0 ~?=  6,
          "2" ~: "valor ej2" ~: 
          valor ej_pol1 1 ~?=  10,
          "3" ~: "valor ej3" ~: 
          valor ej_pol1 2 ~?=  102,
          "4" ~: "valor ej4" ~: 
          valor ej_pol2 0 ~?=  6.5,
          "5" ~: "valor ej5" ~: 
          valor ej_pol2 1 ~?=  10.3,
          "6" ~: "valor ej6" ~: 
          valor ej_pol2 3 ~?=  532.7]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
