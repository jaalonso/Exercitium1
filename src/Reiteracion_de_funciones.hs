-- Reiteracion_de_funciones.hs
-- Reiteración de una función.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 10 de Mayo de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función 
--    reiteracion :: Int -> (a -> a) -> a -> a
-- tal que (reiteracion n f x) es el resultado de aplicar n veces la
-- función f a x. Por ejemplo,
--    reiteracion 10 (+1) 5  ==  15
--    reiteracion 10 (+5) 0  ==  50
--    reiteracion  4 (*2) 1  ==  16
--    reiteracion1 4 (5:) [] ==  [5,5,5,5]
-- 
-- Comprobar con QuickCheck que se verifican las siguientes propiedades
--    reiteracion 10 (+1) x  == 10 + x 
--    reiteracion 10 (+x) 0  == 10 * x 
--    reiteracion 10 (x:) [] == replicate 10 x  
-- ---------------------------------------------------------------------

import Test.HUnit
import Test.QuickCheck

-- 1ª definición (por recursión):
reiteracion1 :: Int -> (a -> a) -> a -> a
reiteracion1 0 f x = x
reiteracion1 n f x = f (reiteracion1 (n-1) f x)

-- 2ª definición (por recursión sin el 3ª argumento):
reiteracion2 :: Int -> (a -> a) -> a -> a
reiteracion2 0 f = id
reiteracion2 n f = f . reiteracion2 (n-1) f

-- 3ª definición (con iterate):
reiteracion3 :: Int -> (a -> a) -> a -> a
reiteracion3 n f x = (iterate f x) !! n
