-- Entero_positivo_de_la_cadena.hs
-- Entero positivo de la cadena.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    enteroPositivo :: String -> Maybe Int
-- tal que (enteroPositivo cs) es justo el contenido de la cadena cs, si
-- dicho contenido es un entero positivo, y Nothing en caso contrario. 
-- Por ejemplo, 
--    enteroPositivo "235"    ==  Just 235
--    enteroPositivo "-235"   ==  Nothing
--    enteroPositivo "23.5"   ==  Nothing
--    enteroPositivo "235 "   ==  Nothing
--    enteroPositivo "cinco"  ==  Nothing
--    enteroPositivo ""       ==  Nothing
-- ---------------------------------------------------------------------

import Data.Maybe (listToMaybe)
import Numeric (readDec)
import Data.Char (isDigit)
import Data.List (nub,intersect) -- Para A1
import Data.Char (ord)           -- Para A1

-- 1ª definición de enteroPositivo1:
enteroPositivo :: String -> Maybe Int
enteroPositivo ""                   = Nothing
enteroPositivo cs | todosDigitos cs = Just (read cs)
                  | otherwise       = Nothing

-- (todosDigitos cs) se verifica si todos los elementos de cs son
-- dígitos. Por ejemplo,
--    todosDigitos "235"    ==  True
--    todosDigitos "-235"   ==  False
--    todosDigitos "23.5"   ==  False
--    todosDigitos "235 "   ==  False
--    todosDigitos "cinco"  ==  False

-- (esDigito c) se verifica si el carácter c es un dígito. Por ejemplo,
--    esDigito '5'  ==  True
--    esDigito 'a'  ==  False

-- 1ª definición de esDigito:
esDigito1 :: Char -> Bool
esDigito1 c = c `elem` "0123456789"

-- 2ª definición de esDigito:
esDigito2 :: Char -> Bool
esDigito2 c = c `elem` ['0'..'9']

-- 3ª definición de esDigito:
esDigito3 :: Char -> Bool
esDigito3 = (`elem` ['0'..'9'])

-- 4ª definición de esDigito:
esDigito4 :: Char -> Bool
esDigito4 c = '0' <= c && c <= '9'

-- 5ª definición de esDigito:
esDigito5 :: Char -> Bool
esDigito5 = isDigit

-- Usaremos como definición de esDigito la 5ª:
esDigito :: Char -> Bool
esDigito = esDigito5

-- 1ª definición de todosDigitos (por comprensión):
todosDigitos :: String -> Bool
todosDigitos cs = and [esDigito c | c <- cs]

-- 2ª definición de todosDigitos (por recursión):
todosDigitos2 :: String -> Bool
todosDigitos2 []     = True
todosDigitos2 (c:cs) = esDigito c && todosDigitos2 cs

-- 3ª definición de todosDigitos (por recursión):
todosDigitos3 :: String -> Bool
todosDigitos3 = foldr ((&&) . esDigito) True

-- 4ª definición de todosDigitos (con all):
todosDigitos4 :: String -> Bool
todosDigitos4 = all esDigito

-- 2ª definición de enteroPositivo:
enteroPositivo2 :: String -> Maybe Int
enteroPositivo2 cs | null xs   = Nothing
                   | otherwise = Just (head xs)
                   where xs = [x | (x,y) <- readDec cs, null y]

-- Nota. En la definición anterior se ha usado la función readDec de la
-- librería Numeric. El valor de (readDec cs) es la lista de los pares
-- (x,y) tales que x es el entero positivo al principio de cs e y es el
-- resto. Por ejemplo,  
--    readDec "235"    ==  [(235,"")]
--    readDec "-235"   ==  []
--    readDec "23.5"   ==  [(23,".5")]
--    readDec "235 "   ==  [(235," ")]
--    readDec "cinco"  ==  []
--    readDec ""       ==  []

-- 3ª definición de enteroPositivo (sin argumentos):
enteroPositivo3 :: String -> Maybe Int
enteroPositivo3 =
    fmap fst . listToMaybe . filter (null . snd) . readDec

-- Nota. En la definición anterior se ha usado la función listToMaybe
-- (de la librería Data.Maybe) tal que (listToMaybe xs) es Nothing si xs
-- es la lista vacía o (Just x) donde x es el primer elemento de xs. Por
-- ejemplo, 
--    listToMaybe []       ==  Nothing
--    listToMaybe [3,2,5]  ==  Just 3
-- y la función fmap tal que (fmap f x) le aplica la función f a los
-- elementos de x. Por ejemplo,
--    fmap (+2) (Just 3)  ==  Just 5
--    fmap (+2) Nothing   ==  Nothing
--    fmap (+2) [3,4,6]   ==  [5,6,8]
--    fmap (+2) []        ==  []
 
-- Nota. Ejemplos de cálculo con enteroPositivo3
--    enteroPositivo3 "325"
--    = (fmap fst . listToMaybe . filter (null . snd) . readDec) "325"
--    = (fmap fst . listToMaybe . filter (null . snd)) [(325,"")]
--    = (fmap fst . listToMaybe) [(325,"")]
--    = fmap fst (Just (325,""))
--    = Just 325
-- 
--    enteroPositivo3 "32.5"
--    = (fmap fst . listToMaybe . filter (null . snd) . readDec) "32.5"
--    = (fmap fst . listToMaybe . filter (null . snd)) [(32,".5")]
--    = (fmap fst . listToMaybe ) []
--    = fmap fst Nothing
--    = Nothing
