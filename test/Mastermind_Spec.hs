module Mastermind_Spec (main, spec) where

import Mastermind
import Data.List (nub)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "mastermind" $ do
    describe "Ejemplos" $ do
      it "e1" $ 
        mastermind [2,6,0,7] [1,4,0,6]  `shouldBe`  (1,1)
      it "e2" $ 
        mastermind [2,6,0,7] [3,5,9,1]  `shouldBe`  (0,0)
      it "e3" $ 
        mastermind [2,6,0,7] [1,6,0,4]  `shouldBe`  (2,0)
      it "e4" $ 
        mastermind [2,6,0,7] [2,6,0,7]  `shouldBe`  (4,0)
    describe "Propiedades" $ do
      it "equivalencia" $ 
        property $ prop_equivalencia

-- Equivalencia de las definiciones
prop_equivalencia :: [Int] -> [Int] -> Property
prop_equivalencia xs ys =
  length xs1 > 6 && length ys1 > 6 ==>
  mastermind2 xs2 ys2 == r &&
  mastermind3 xs2 ys2 == r 
  where xs1 = nub (map abs xs)
        ys1 = nub (map abs ys)
        xs2 = take 7 xs1
        ys2 = take 7 ys1
        r   = mastermind xs2 ys2 
