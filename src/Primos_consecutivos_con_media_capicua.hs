-- Primos_consecutivos_con_media_capicua.hs
-- Primos consecutivos con media capicúa.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 28 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La pasada semana, Antonio Roldán publicó en
-- [Twitter](http://bit.ly/1mPSepM) la siguiente observación:
--    Los pares de primos consecutivos (97,101) y (109,113) son los más
--    pequeños, con promedio capicúa con más de una cifra: (97+101)/2=99
--    y (109+113)/2=111.  
-- A partir de ella, se propone el ejercicio de hoy.
--
-- Definir la constante
--    primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
-- tal que primosConsecutivosConMediaCapicua es la lista de las ternas 
-- (x,y,z) tales que x e y son primos consecutivos tales que su media,
-- z, es capicúa. Por ejemplo,
--    ghci> take 5 primosConsecutivosConMediaCapicua
--    [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
-- Calcular cuántos hay anteriores a 2014.
-- ---------------------------------------------------------------------

import Data.Numbers.Primes (primes)

-- 1ª definición
primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua =
  [(x,y,z) | (x,y) <- zip primos (tail primos)
           , let z = (x + y) `div` 2
           , capicua z]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Int -> Bool
primo x = [y | y <- [1..x]
             , x `rem` y == 0] == [1,x]

-- primos es la lista de los números primos mayores que 2. Por ejemplo,
--    take 10 primos  ==  [3,5,7,11,13,17,19,23,29]
primos :: [Int]
primos = [x | x <- [3,5..]
            , primo x]

-- (capicua x) se verifica si x es capicúa. Por ejemplo,
capicua :: Int -> Bool
capicua x = ys == reverse ys
  where ys = show x

-- 2ª definición
primosConsecutivosConMediaCapicua2 :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua2 =
  [(x,y,z) | (x,y) <- zip primos3 (tail primos3)
           , let z = (x + y) `div` 2
           , capicua z]
  where primos3 = tail primes

-- El cálculo es
--    ghci> length (takeWhile (\(x,y,z) -> y < 2014) primosConsecutivosConMediaCapicua)
--    20

-- Comparación de eficiencia:
--    λ> primosConsecutivosConMediaCapicua !! 30
--    (12919,12923,12921)
--    (2.91 secs, 1,887,929,216 bytes)
--    λ> primosConsecutivosConMediaCapicua2 !! 30
--    (12919,12923,12921)
--    (0.01 secs, 0 bytes)

