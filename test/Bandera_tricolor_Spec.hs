module Bandera_tricolor_Spec (main, spec) where

import Test.Hspec
import Bandera_tricolor

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "banderaTricolor" $ do
    describe "banderaTricolor" $ do
      it "e1" $ 
        banderaTricolor [M,R,A,A,R,R,A,M,M] `shouldBe` [R,R,R,A,A,A,M,M,M]
      it "e2" $ 
        banderaTricolor [M,R,A,R,R,A] `shouldBe` [R,R,R,A,A,M]
    describe "banderaTricolor2" $ do
      it "e1" $ 
        banderaTricolor2 [M,R,A,A,R,R,A,M,M] `shouldBe` [R,R,R,A,A,A,M,M,M]
      it "e2" $ 
        banderaTricolor2 [M,R,A,R,R,A] `shouldBe` [R,R,R,A,A,M]
    describe "banderaTricolor3" $ do
      it "e1" $ 
        banderaTricolor3 [M,R,A,A,R,R,A,M,M] `shouldBe` [R,R,R,A,A,A,M,M,M]
      it "e2" $ 
        banderaTricolor3 [M,R,A,R,R,A] `shouldBe` [R,R,R,A,A,M]
    describe "banderaTricolor5" $ do
      it "e1" $ 
        banderaTricolor4 [M,R,A,A,R,R,A,M,M] `shouldBe` [R,R,R,A,A,A,M,M,M]
      it "e2" $ 
        banderaTricolor4 [M,R,A,R,R,A] `shouldBe` [R,R,R,A,A,M]
    describe "banderaTricolor5" $ do
      it "e1" $ 
        banderaTricolor5 [M,R,A,A,R,R,A,M,M] `shouldBe` [R,R,R,A,A,A,M,M,M]
      it "e2" $ 
        banderaTricolor5 [M,R,A,R,R,A] `shouldBe` [R,R,R,A,A,M]
