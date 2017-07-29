-- |
-- Module      : Mayor_producto_de_las_ramas_de_un_arbol
-- Description : Mayor producto de las ramas de un árbol.
-- Copyright   : Exercitium (22-05-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--
-- >    data Arbol a = N a [Arbol a]
-- >                   deriving Show
-- 
-- Por ejemplo, los árboles
-- 
-- >       1              3
-- >     /  \            /|\ 
-- >    2   3           / | \
-- >        |          5  4  7
-- >        4          |     /\ 
-- >                   6    2  1
-- 
-- se representan por
-- 
-- >   ej1, ej2 :: Arbol Int
-- >   ej1 = N 1 [N 2 [],N 3 [N 4 []]]
-- >   ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
-- 
-- > mayorProducto :: (Ord a, Num a) => Arbol a -> a
-- 
-- tal que __(mayorProducto a)__ es el mayor producto de las ramas del árbol
-- a. Por ejemplo,
-- 
-- >>> mayorProducto (N 1 [N 2 [], N  3 []])
-- 3
-- >>> mayorProducto (N 1 [N 8 [], N  4 [N 3 []]])
-- 12
-- >>> mayorProducto (N 1 [N 2 [],N 3 [N 4 []]])
-- 12
-- >>> mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
-- 90
-- >>> mayorProducto (N (-8) [N 0 [N (-9) []],N 6 []])
-- 0
-- >>> let a = N (-4) [N (-7) [],N 14 [N 19 []],N (-1) [N (-6) [],N 21 []],N (-4) []]
-- >>> mayorProducto a
-- 84

module Mayor_producto_de_las_ramas_de_un_arbol where

import Test.QuickCheck

-- | Tipo de árboles
data Arbol a = N a [Arbol a]
  deriving Show

-- | 1º definición.
mayorProducto :: (Ord a, Num a) => Arbol a -> a
mayorProducto a = maximum (productosRamas a)

-- | __(productosRamas a)__ es la lista de los productos de las ramas
-- del árbol a. Por ejemplo,
--
-- >>> productosRamas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
-- [90,12,42,21]
productosRamas :: (Ord a, Num a) => Arbol a -> [a]
productosRamas (N x []) = [x]
productosRamas (N x xs) = [x * y | a <- xs, y <- productosRamas a]

-- | 2ª definición.
mayorProducto2 :: (Ord a, Num a) => Arbol a -> a
mayorProducto2 (N x []) = x
mayorProducto2 (N x xs)
  | x > 0     = x * maximum (map mayorProducto2 xs)
  | x == 0    = 0
  | otherwise = x * minimum (map menorProducto xs)

-- | __(menorProducto a)__ es el menor producto de las ramas del árbol
-- a. Por ejemplo,
-- 
-- >>> menorProducto (N 1 [N 2 [], N  3 []])
-- 2
-- >>> menorProducto (N 1 [N 8 [], N  4 [N 3 []]])
-- 8
-- >>> menorProducto (N 1 [N 2 [],N 3 [N 4 []]])
-- 2
-- >>> menorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
-- 12
menorProducto :: (Ord a, Num a) => Arbol a -> a
menorProducto (N x []) = x
menorProducto (N x xs)
  | x > 0     = x * minimum (map menorProducto xs)
  | x == 0    = 0
  | otherwise = x * maximum (map mayorProducto2 xs)

-- | 3ª definición.
mayorProducto3 :: (Ord a, Num a) => Arbol a -> a
mayorProducto3 a = maximum [product xs | xs <- ramas a]

-- | __(ramas a)__ es la lista de las ramas del árbol a. Por ejemplo,
-- 
-- >>> ramas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
-- [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- | 4ª definición.
mayorProducto4 :: (Ord a, Num a) => Arbol a -> a
mayorProducto4 a = maximum (map product (ramas a))

-- | 5ª definición.
mayorProducto5 :: (Ord a, Num a) => Arbol a -> a
mayorProducto5 = maximum . map product . ramas

-- | Generador de árboles.
--
-- > > sample ((gen_Arbol 5) :: Gen (Arbol Int))
-- > N 0 [N 0 []]
-- > N (-2) []
-- > N 4 []
-- > N 2 [N 4 []]
-- > N 8 []
-- > N (-2) [N (-9) [],N 7 []]
-- > N 11 []
-- > N (-11) [N 4 [],N 14 []]
-- > N 10 [N (-3) [],N 13 []]
-- > N 12 [N 11 []]
-- > N 20 [N (-18) [],N (-13) []]
gen_Arbol :: Arbitrary a => Int -> Gen (Arbol a)
gen_Arbol m = do
  x <- arbitrary
  n <- choose (0, m `div` 2)
  xs <- vectorOf n (gen_Arbol (m `div` 4))
  return (N x xs)

-- | Incluye los árboles en Arbitrary.
instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized gen_Arbol

-- | Comprueba la equivalencia de las definiciones de mayorProducto.
--
-- >>> quickCheck prop_mayorProducto
-- +++ OK, passed 100 tests.
prop_mayorProducto :: Arbol Int -> Bool
prop_mayorProducto a =
  all (== mayorProducto a)
      [f a | f <- [ mayorProducto2
                  , mayorProducto3
                  , mayorProducto4
                  , mayorProducto5
                  ]]
