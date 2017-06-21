-- Bandera_tricolor.hs
-- La bandera tricolor.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema de la bandera tricolor consiste en lo siguiente: Dada un
-- lista de objetos xs que pueden ser rojos, amarillos o morados, se pide
-- devolver una lista ys que contiene los elementos de xs, primero los
-- rojos, luego los amarillos y por último los morados. 
-- 
-- Definir el tipo de dato Color para representar los colores con los
-- constructores R, A y M correspondientes al rojo, azul y morado y la
-- función 
--    banderaTricolor :: [Color] -> [Color]
-- tal que (banderaTricolor xs) es la bandera tricolor formada con los
-- elementos de xs. Por ejemplo,
--    banderaTricolor [M,R,A,A,R,R,A,M,M]  ==  [R,R,R,A,A,A,M,M,M]
--    banderaTricolor [M,R,A,R,R,A]        ==  [R,R,R,A,A,M]
-- ---------------------------------------------------------------------

module Bandera_tricolor where

import Data.List (sort)

data Color = R | A | M
  deriving (Show, Eq, Ord, Enum)

-- 1ª definición (con sort):
banderaTricolor :: [Color] -> [Color]
banderaTricolor = sort

-- 2ª definición (por comprensión):
banderaTricolor2 :: [Color] -> [Color]
banderaTricolor2 xs =
    [x | x <- xs, x == R] ++ [x | x <- xs, x == A] ++ [x | x <- xs, x == M]

-- 3ª definición (por comprensión y concat):
banderaTricolor3 :: [Color] -> [Color]
banderaTricolor3 xs =
    concat [[x | x <- xs, x == c] | c <- [R,A,M]]

-- 4ª definición (por recursión):
banderaTricolor4 :: [Color] -> [Color]
banderaTricolor4 xs = aux xs ([],[],[])
    where aux []     (rs,as,ms) = rs ++ as ++ ms
          aux (R:ys) (rs,as,ms) = aux ys (R:rs,   as,   ms)
          aux (A:ys) (rs,as,ms) = aux ys (  rs, A:as,   ms)
          aux (M:ys) (rs,as,ms) = aux ys (  rs,   as, M:ms)

-- 5ª definición (por recursión):
banderaTricolor5 :: [Color] -> [Color]
banderaTricolor5 xs = aux xs (0,0,0)
    where aux []     (as,rs,ms) = replicate rs R ++ 
                                  replicate as A ++
                                  replicate ms M
          aux (A:ys) (as,rs,ms) = aux ys (1+as,   rs,   ms)
          aux (R:ys) (as,rs,ms) = aux ys (  as, 1+rs,   ms)
          aux (M:ys) (as,rs,ms) = aux ys (  as,   rs, 1+ms)

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
