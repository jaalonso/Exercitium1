-- Limite.hs
-- Límite de sucesiones.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla,  1 de Julio de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Definir la función  
--    limite :: (Double -> Double) -> Double -> Double
-- tal que (limite f a) es el valor de f en el primer término x tal que, 
-- para todo y entre x+1 y x+100, el valor absoluto de la diferencia
-- entre f(y) y f(x) es menor que a. Por ejemplo,
--    limite (\n -> (2*n+1)/(n+5)) 0.001  ==  1.9900110987791344
--    limite (\n -> (1+1/n)**n) 0.001     ==  2.714072874546881
-- ---------------------------------------------------------------------

limite1 :: (Double -> Double) -> Double -> Double
limite1 f a = 
    head [f x | x <- [1..],
                maximum [abs(f y - f x) | y <- [x+1..x+100]] < a]

-- ---------------------------------------------------------------------
-- § Soluciones de alumnos                                            --
-- ---------------------------------------------------------------------

-- Paco
-- ====
limiteA1 :: (Double -> Double) -> Double -> Double
limiteA1 f a = head [f x | x <- [1..], p f x a]
      where p f x a = and [abs((f x)-(f y)) < a | y <- [x+1..x+100]] 

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica f =
    limite (\n -> (2*n+1)/(n+5)) 0.001  ==  1.9900110987791344 &&
    limite (\n -> (1+1/n)**n) 0.001     ==  2.714072874546881
    where limite = f
