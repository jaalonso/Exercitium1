-- Mastermind.hs
-- Mastermind.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 25 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El Mastermind es un juego que consiste en deducir un código
-- numérico formado por una lista de números distintos. Cada vez que se
-- empieza un partido, el programa debe elegir un código, que será lo
-- que el jugador debe adivinar en la menor cantidad de intentos
-- posibles. Cada intento consiste en una propuesta de un código posible
-- que propone el jugador, y una respuesta del programa. Las respuestas
-- le darán pistas al jugador para que pueda deducir el código.
-- 
-- Estas pistas indican cuán cerca estuvo el número propuesto de la
-- solución a través de dos valores: la cantidad de aciertos es la
-- cantidad de dígitos que propuso el jugador que también están en el
-- código en la misma posición. La cantidad de coincidencias es la
-- cantidad de digitos que propuso el jugador que también están en el
-- código pero en una posición distinta. 
-- 
-- Por ejemplo, si el código que eligió el programa es el [2,6,0,7], y
-- el jugador propone el [1,4,0,6], el programa le debe responder un
-- acierto (el 0, que está en el código original en el mismo lugar, el
-- tercero), y una coincidencia (el 6, que también está en el código
-- original, pero en la segunda posición, no en el cuarto como fue
-- propuesto). Si el jugador hubiera propuesto el [3,5,9,1], habría
-- obtenido como respuesta ningún acierto y ninguna coincidencia, ya que
-- no hay números en común con el código original, y si se obtienen
-- cuatro aciertos es porque el jugador adivinó el código y ganó el
-- juego.  
-- 
-- Definir la función
--    mastermind :: [Int] -> [Int] -> (Int,Int)
-- tal que (mastermind xs ys) es el par formado por los números de
-- aciertos y de coincidencias entre xs e ys. Por ejemplo,
--    mastermind [2,6,0,7] [1,4,0,6]  ==  (1,1)
--    mastermind [2,6,0,7] [3,5,9,1]  ==  (0,0)
--    mastermind [2,6,0,7] [1,6,0,4]  ==  (2,0)
--    mastermind [2,6,0,7] [2,6,0,7]  ==  (4,0)
-- ---------------------------------------------------------------------

module Mastermind where

import Data.List (nub)

-- 1ª solución (por comprensión):
mastermind :: [Int] -> [Int] -> (Int,Int)
mastermind xs ys = 
  ( length (aciertos xs ys)
  , length (coincidencias xs ys))

-- (aciertos xs ys) es la lista de aciertos entre xs e ys. Por ejemplo,
--    aciertos [2,6,0,7] [1,4,0,6]  ==  [0]
aciertos :: Eq a => [a] -> [a] -> [a]
aciertos xs ys = [x | (x,y) <- zip xs ys
                    , x == y]

-- (coincidencia xs ys) es la lista de coincidencias entre xs e ys. Por
-- ejemplo, 
--    coincidencias [2,6,0,7] [1,4,0,6]  ==  [6]
coincidencias :: Eq a => [a] -> [a] -> [a]
coincidencias xs ys = 
  [x | x <- xs
     , x `elem` ys
     , x `notElem` zs]
  where zs = aciertos xs ys

-- 2ª solución (por recursión):
mastermind2 :: [Int] -> [Int] -> (Int,Int)
mastermind2 xs ys = aux xs ys 
  where aux (u:us) (z:zs) 
          | u == z      = (a+1,b)
          | u `elem` ys = (a,b+1)
          | otherwise   = (a,b)
          where (a,b) = aux us zs
        aux _ _         = (0,0)
        
-- 3ª solución:
mastermind3 :: [Int] -> [Int] -> (Int,Int)
mastermind3 xs ys = (nAciertos,nCoincidencias)
  where
    nAciertos      = length [(x,y) | (x,y) <- zip xs ys, x == y]
    nCoincidencias = length (xs++ys) - length (nub (xs++ys)) - nAciertos
