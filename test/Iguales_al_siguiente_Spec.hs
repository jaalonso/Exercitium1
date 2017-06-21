module Iguales_al_siguiente_Spec (main, spec) where

import Test.Hspec
import Iguales_al_siguiente

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "igualesAlSiguiente" $ do
    describe "igualesAlSiguiente" $ do
      it "e1" $ 
        igualesAlSiguiente [1::Int,2,2,2,3,3,4] `shouldBe` [2,2,3]
      it "e2" $ 
        igualesAlSiguiente [1..10::Int] `shouldBe`  []
    describe "igualesAlSiguiente2" $ do
      it "e1" $ 
        igualesAlSiguiente2 [1::Int,2,2,2,3,3,4] `shouldBe` [2,2,3]
      it "e2" $ 
        igualesAlSiguiente2 [1..10::Int] `shouldBe`  []
    describe "igualesAlSiguiente3" $ do
      it "e1" $ 
        igualesAlSiguiente3 [1::Int,2,2,2,3,3,4] `shouldBe` [2,2,3]
      it "e2" $ 
        igualesAlSiguiente3 [1..10::Int] `shouldBe`  []
    describe "igualesAlSiguiente4" $ do
      it "e1" $ 
        igualesAlSiguiente4 [1::Int,2,2,2,3,3,4] `shouldBe` [2,2,3]
      it "e2" $ 
        igualesAlSiguiente4 [1..10::Int] `shouldBe`  []
    describe "igualesAlSiguiente5" $ do
      it "e1" $ 
        igualesAlSiguiente5 [1::Int,2,2,2,3,3,4] `shouldBe` [2,2,3]
      it "e2" $ 
        igualesAlSiguiente5 [1..10::Int] `shouldBe`  []

