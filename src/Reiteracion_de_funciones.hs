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

-- ---------------------------------------------------------------------
-- § Soluciones de los alumnos                                        --
-- ---------------------------------------------------------------------

-- Luis
reiteracionA1 :: Int -> (a -> a) -> a -> a
reiteracionA1 n f x = (iterate f x) !! n
 
-- prop_Reiteracion x = reiteracion 10 (+1) x  == 10 + x 
-- *Main> quickCheck prop_Reiteracion
-- +++ OK, passed 100 tests.
 
-- prop_Reiteracion x = reiteracion 10 (+x) 0  == 10 * x 
 -- *Main> quickCheck prop_Reiteracion
-- +++ OK, passed 100 tests.
 
-- prop_Reiteracion x = reiteracion 10 (x:) [] == replicate 10 x
-- *Main> quickCheck prop_Reiteracion
-- +++ OK, passed 100 tests.

-- Laura
-- =====

reiteracionA2 :: Int -> (a -> a) -> a -> a
reiteracionA2 n f x = last (take (n+1) (iterate f x))

propiedad1 :: (Eq a, Num a) => t -> t1 -> a -> Bool
propiedad1 n f x = reiteracion 10 (+1) x == 10 + x

-- *Main> quickCheck propiedad1
-- +++ OK, passed 100 tests.
-- (0.02 secs, 3682832 bytes)

propiedad2 :: (Eq a, Num a) => t -> t1 -> a -> Bool
propiedad2 n f x = reiteracion 10 (+x) 0 == 10 * x

-- *Main> quickCheck propiedad2
-- +++ OK, passed 100 tests.
-- (0.02 secs, 3628792 bytes)

propiedad3 :: Eq a => t -> t1 -> a -> Bool
propiedad3 n f x = reiteracion 10 (x:) [] == replicate 10 x

-- *Main> quickCheck propiedad3
-- +++ OK, passed 100 tests.
-- (0.02 secs, 3120636 bytes)

-- Ángela
-- ======

reiteracionA3 :: Int -> (a -> a) -> a -> a
reiteracionA3 0 f x = x
reiteracionA3 n f x = reiteracionA3 (n-1) f (f x)

reiteracionA3' :: Int -> (a -> a) -> a -> a
reiteracionA3' 0 f = id
reiteracionA3' n f = f . reiteracionA3' (n-1) f
 
propReitera1 x = reiteracionA3 10 (+1) x  == 10 + x 
-- +++ OK, passed 100 tests.
 
propReitera2 x = reiteracionA3 10 (+x) 0  == 10 * x 
-- +++ OK, passed 100 tests.
 
propReitera3 x = reiteracionA3 10 (x:) [] == replicate 10 x
-- +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

reiteracion :: Int -> (a -> a) -> a -> a
reiteracion = reiteracionA3'

ejemplos :: Test
ejemplos =
    test ["1" ~: "ej1" ~: 
          reiteracion 10 (+1) 5  ~?=  15,
          "2" ~: "ej2" ~: 
          reiteracion 10 (+5) 0  ~?=  50,
          "3" ~: "ej3" ~: 
          reiteracion  4 (*2) 1  ~?=  16,
          "4" ~: "ej4" ~: 
          reiteracion1 4 (5:) [] ~?=  [5,5,5,5]]

verifica = runTestTT ejemplos

-- Verificación:
--    ghci> verifica 
--    Cases: 2  Tried: 2  Errors: 0  Failures: 0
--    Counts {cases = 2, tried = 2, errors = 0, failures = 0}
prop_suma :: Int -> Bool
prop_suma x = 
    reiteracion 10 (+1) x  == 10 + x 

prop_producto :: Int -> Bool
prop_producto x = 
    reiteracion 10 (+x) 0  == 10 * x 

prop_cons :: Int -> Bool
prop_cons x = 
    reiteracion 10 (x:) [] == replicate 10 x  

-- Las comprobaciones son
--    ghci> quickCheck prop_suma
--    +++ OK, passed 100 tests.
--    ghci> quickCheck prop_producto
--    +++ OK, passed 100 tests.
--    ghci> quickCheck prop_cons
--    +++ OK, passed 100 tests.
