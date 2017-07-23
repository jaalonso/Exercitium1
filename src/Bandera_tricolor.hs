-- |
-- Module      : Bandera_tricolor.hs 
-- Description : La bandera tricolor.
-- Copyright   : José A. Alonso (23 de Abril de 2014)
-- License     : GPL-3
-- 
-- El problema de la bandera tricolor consiste en lo siguiente: Dada un
-- lista de objetos xs que pueden ser rojos, amarillos o morados, se pide
-- devolver una lista ys que contiene los elementos de xs, primero los
-- rojos, luego los amarillos y por último los morados. 
-- 
-- Definir el tipo de dato Color para representar los colores con los
-- constructores R, A y M correspondientes al rojo, azul y morado y la
-- función
--
-- > banderaTricolor :: [Color] -> [Color]
-- 
-- tal que (banderaTricolor xs) es la bandera tricolor formada con los
-- elementos de xs. Por ejemplo,
--
-- >>> banderaTricolor [M,R,A,A,R,R,A,M,M]
-- [R,R,R,A,A,A,M,M,M]
-- >>> banderaTricolor [M,R,A,R,R,A]
-- [R,R,R,A,A,M]

module Bandera_tricolor
  ( Color
  , banderaTricolor
  , verifica_banderaTricolor
  ) where

import Data.List (sort)
import Test.QuickCheck

data Color = R | A | M
  deriving (Show, Eq, Ord, Enum)

banderaTricolor :: [Color] -> [Color]
banderaTricolor = sort

-- | 2ª definición (por comprensión):
banderaTricolor2 :: [Color] -> [Color]
banderaTricolor2 xs =
    [x | x <- xs, x == R] ++ [x | x <- xs, x == A] ++ [x | x <- xs, x == M]

-- | 3ª definición (por comprensión y concat):
banderaTricolor3 :: [Color] -> [Color]
banderaTricolor3 xs =
    concat [[x | x <- xs, x == c] | c <- [R,A,M]]

-- | 4ª definición (por recursión):
banderaTricolor4 :: [Color] -> [Color]
banderaTricolor4 xs = aux xs ([],[],[])
    where aux []     (rs,as,ms) = rs ++ as ++ ms
          aux (R:ys) (rs,as,ms) = aux ys (R:rs,   as,   ms)
          aux (A:ys) (rs,as,ms) = aux ys (  rs, A:as,   ms)
          aux (M:ys) (rs,as,ms) = aux ys (  rs,   as, M:ms)

-- | 5ª definición (por recursión):
banderaTricolor5 :: [Color] -> [Color]
banderaTricolor5 xs = aux xs (0,0,0)
    where aux []     (as,rs,ms) = replicate rs R ++ 
                                  replicate as A ++
                                  replicate ms M
          aux (A:ys) (as,rs,ms) = aux ys (1+as,   rs,   ms)
          aux (R:ys) (as,rs,ms) = aux ys (  as, 1+rs,   ms)
          aux (M:ys) (as,rs,ms) = aux ys (  as,   rs, 1+ms)

-- | (prop_banderaTricolor xs) se verifica si todas las definiciones
-- de banderaTricolor son equivalentes para xs. Por ejemplo,
--
-- >>> prop_banderaTricolor [M,R,A,A,R,R,A,M,M]
-- True
-- >>> prop_banderaTricolor [M,R,A,R,R,A]
-- True
prop_banderaTricolor :: [Color] -> Bool
prop_banderaTricolor xs =
  all (== banderaTricolor xs)
      [f xs | f <- [ banderaTricolor2
                   , banderaTricolor3
                   , banderaTricolor4
                   , banderaTricolor5]]

-- | Incluye el tipo Color en Arbitrary
instance Arbitrary Color where
  arbitrary = elements [A,R,M]

-- | Comprueba la equivalencia de las definiciones
--
-- >>> verifica_banderaTricolor
-- +++ OK, passed 100 tests.
verifica_banderaTricolor :: IO ()
verifica_banderaTricolor = 
  quickCheck prop_banderaTricolor

-- Comparaciones:
--    ghci> let bandera n = concat [replicate n c | c <- [M,R,A]]
--    
--    ghci> :set +s
--    
--    ghci> length (banderaTricolor (bandera 1000000))
--    3000000
--    (2.65 secs, 312128100 bytes)
--    
--    ghci> length (banderaTricolor2 (bandera 1000000))
--    3000000
--    (7.78 secs, 512387912 bytes)
--    
--    ghci> length (banderaTricolor3 (bandera 1000000))
--    3000000
--    (7.84 secs, 576080444 bytes)
--    
--    ghci> length (banderaTricolor4 (bandera 1000000))
--    3000000
--    (3.76 secs, 476484220 bytes)
--    
--    ghci> length (banderaTricolor5 (bandera 1000000))
--    3000000
--    (4.45 secs, 622205356 bytes)

-- Comprobación
--    > stack exec doctest Bandera_tricolor.hs
--    Examples: 5  Tried: 5  Errors: 0  Failures: 0

