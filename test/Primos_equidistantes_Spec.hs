module Primos_equidistantes_Spec (main, spec) where

import Primos_equidistantes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "primosEquidistantes" $ do
    describe "primosEquidistantes" $ do
      it "e1" $ 
        take 3 (primosEquidistantes 2) `shouldBe` [(3,5),(5,7),(11,13)]
      it "e2" $ 
        take 3 (primosEquidistantes 4) `shouldBe` [(7,11),(13,17),(19,23)]
      it "e3" $ 
        take 3 (primosEquidistantes 6) `shouldBe` [(23,29),(31,37),(47,53)]
      it "e4" $ 
        take 3 (primosEquidistantes 8) `shouldBe` [(89,97),(359,367),(389,397)]
    describe "primosEquidistantes2" $ do
      it "e1" $ 
        take 3 (primosEquidistantes2 2) `shouldBe` [(3,5),(5,7),(11,13)]
      it "e2" $ 
        take 3 (primosEquidistantes2 4) `shouldBe` [(7,11),(13,17),(19,23)]
      it "e3" $ 
        take 3 (primosEquidistantes2 6) `shouldBe` [(23,29),(31,37),(47,53)]
      it "e4" $ 
        take 3 (primosEquidistantes2 8) `shouldBe` [(89,97),(359,367),(389,397)]
    
