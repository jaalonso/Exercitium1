-- |
-- Module      : Ordenacion_de_estructuras
-- Description : Ordenación de estructuras.
-- Copyright   : Exercitium (14-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Las notas de los dos primeros exámenes se pueden representar mediante
-- el siguiente tipo de dato
--
-- > data Notas = Notas String Int Int
-- >              deriving (Read, Show, Eq)
-- 
-- Por ejemplo, (Notas "Juan" 6 5) representar las notas de un alumno
-- cuyo nombre es Juan, la nota del primer examen es 6 y la del segundo
-- es 5.
-- 
-- Definir la función
-- 
-- > ordenadas :: [Notas] -> [Notas]
-- 
-- tal que __(ordenadas ns)__ es la lista de las notas ns ordenadas
-- considerando primero la nota del examen 2, a continuación la del
-- examen 1 y finalmente el nombre. Por ejemplo,
-- 
-- >>> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 7] 
-- [Notas "Juan" 6 5,Notas "Luis" 3 7]
-- >>> ordenadas [Notas "Juan" 6 5, Notas "Luis" 3 4] 
-- [Notas "Luis" 3 4,Notas "Juan" 6 5]
-- >>> ordenadas [Notas "Juan" 6 5, Notas "Luis" 7 4] 
-- [Notas "Luis" 7 4,Notas "Juan" 6 5]
-- >>> ordenadas [Notas "Juan" 6 4, Notas "Luis" 7 4] 
-- [Notas "Juan" 6 4,Notas "Luis" 7 4]
-- >>> ordenadas [Notas "Juan" 6 4, Notas "Luis" 5 4] 
-- [Notas "Luis" 5 4,Notas "Juan" 6 4]
-- >>> ordenadas [Notas "Juan" 5 4, Notas "Luis" 5 4] 
-- [Notas "Juan" 5 4,Notas "Luis" 5 4]
-- >>> ordenadas [Notas "Juan" 5 4, Notas "Eva" 5 4] 
-- [Notas "Eva" 5 4,Notas "Juan" 5 4]

module Ordenacion_de_estructuras where

import Data.List (sort)

-- | Tipo se las notas.
data Notas = Notas String Int Int
  deriving (Read, Show, Eq)

-- | Definición.
ordenadas :: [Notas] -> [Notas]
ordenadas ns =
  [Notas n x y | (y,x,n) <- sort [(y1,x1,n1) | (Notas n1 x1 y1) <- ns]] 
