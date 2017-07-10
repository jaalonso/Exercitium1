module Suma_si_todos_justos_Spec (main, spec) where

import Suma_si_todos_justos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "sumaSiTodosJustos" $ do
    describe "sumaSiTodosJustos" $ do
      it "e1" $ 
        sumaSiTodosJustos [Just 2, Just 5] `shouldBe` Just 7
      it "e2" $ 
        sumaSiTodosJustos [Just 2, Just 5, Nothing] `shouldBe` Nothing
    describe "sumaSiTodosJustos2" $ do
      it "e1" $ 
        sumaSiTodosJustos2 [Just 2, Just 5] `shouldBe` Just 7
      it "e2" $ 
        sumaSiTodosJustos2 [Just 2, Just 5, Nothing] `shouldBe` Nothing
    describe "sumaSiTodosJustos3" $ do
      it "e1" $ 
        sumaSiTodosJustos3 [Just 2, Just 5] `shouldBe` Just 7
      it "e2" $ 
        sumaSiTodosJustos3 [Just 2, Just 5, Nothing] `shouldBe` Nothing
    describe "sumaSiTodosJustos4" $ do
      it "e1" $ 
        sumaSiTodosJustos4 [Just 2, Just 5] `shouldBe` Just 7
      it "e2" $ 
        sumaSiTodosJustos4 [Just 2, Just 5, Nothing] `shouldBe` Nothing
    describe "sumaSiTodosJustos5" $ do
      it "e1" $ 
        sumaSiTodosJustos5 [Just 2, Just 5] `shouldBe` Just 7
      it "e2" $ 
        sumaSiTodosJustos5 [Just 2, Just 5, Nothing] `shouldBe` Nothing
    describe "sumaSiTodosJustos6" $ do
      it "e1" $ 
        sumaSiTodosJustos6 [Just 2, Just 5] `shouldBe` Just 7
      it "e2" $ 
        sumaSiTodosJustos6 [Just 2, Just 5, Nothing] `shouldBe` Nothing
