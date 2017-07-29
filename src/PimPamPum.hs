-- |
-- Module      : PimPamPum
-- Description : Pim, Pam, Pum y divisibilidad.
-- Copyright   : Exercitium (01-06-14)
-- License     : GPL-3
-- Maintainer  : JoseA.Alonso@gmail.com
-- 
-- __Pim, Pam, Pum y divisibilidad__
-- 
-- Definir la función
-- 
-- > sonido :: Int -> String
-- 
-- tal que __(sonido n)__ escribe "Pim" si n es divisible por 3, además
-- escribe "Pam" si n es divisible por 5 y también escribe "Pum" si n es
-- divisible por 7. Por ejemplo,
-- 
-- >>> sonido   3
-- "Pim"
-- >>> sonido   5
-- "Pam"
-- >>> sonido   7
-- "Pum"
-- >>> sonido   8
-- ""
-- >>> sonido   9
-- "Pim"
-- >>> sonido  15
-- "PimPam"
-- >>> sonido  21
-- "PimPum"
-- >>> sonido  35
-- "PamPum"
-- >>> sonido 105
-- "PimPamPum"

module PimPamPum where

import Test.QuickCheck

-- | 1ª solución.
sonido :: Int -> String
sonido x = concat [z | (y,z) <- zs, y == 0]
  where xs = [rem x 3, rem x 5, rem x 7]
        zs = zip xs ["Pim","Pam","Pum"]  

-- | 2ª solución.
sonido2 :: Int -> String
sonido2 n = concat (["Pim" | rem n 3 == 0] ++ 
                    ["Pam" | rem n 5 == 0] ++ 
                    ["Pum" | rem n 7 == 0])

-- | 3ª solución.
sonido3 :: Int -> String
sonido3 n = f 3 "Pim" ++ f 5 "Pam" ++ f 7 "Pum"
  where f x c = if rem n x == 0
                then c
                else "" 

-- | 4ª solución.
sonido4 :: Int -> String
sonido4 n = 
  concat [s | (s,d) <- zip ["Pim","Pam","Pum"] [3,5,7], rem n d == 0]

-- | __(prop_equiv_sonido x)__ se verifica si las definiciones de
-- 'sonido' son equivalentes sobre x. Por ejemplo,
--
-- >>> all prop_equiv_sonido [3,5,7,9,15,21,35,105]
-- True
prop_equiv_sonido :: Int -> Bool
prop_equiv_sonido x =
  all (== sonido x)
      [f x | f <- [ sonido2
                  , sonido3
                  , sonido4
                  ]]

-- | Comprueba la equivalencia de las definiciones de 'sonido'.
-- 
-- >>> verifica_equiv_sonido
-- +++ OK, passed 100 tests.
verifica_equiv_sonido :: IO ()
verifica_equiv_sonido =
  quickCheck prop_equiv_sonido
