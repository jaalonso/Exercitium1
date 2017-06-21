module Ordenados_por_maximo_Spec (main, spec) where

import Test.Hspec
import Ordenados_por_maximo

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ordenadosPorMaximo" $ do
    describe "ordenadosPorMaximo" $ do
      it "e1" $ 
        ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
        `shouldBe` [[3::Int,2],[1,4],[6,7,5]]
      it "e2" $ 
        ordenadosPorMaximo ["este","es","el","primero"]
        `shouldBe` ["el","primero","es","este"]
    describe "ordenadosPorMaximo2" $ do
      it "e1" $ 
        ordenadosPorMaximo2 [[3,2],[6,7,5],[1,4]]
        `shouldBe` [[3::Int,2],[1,4],[6,7,5]]
      it "e2" $ 
        ordenadosPorMaximo2 ["este","es","el","primero"]
        `shouldBe` ["el","primero","es","este"]
    describe "ordenadosPorMaximo3" $ do
      it "e1" $ 
        ordenadosPorMaximo3 [[3,2],[6,7,5],[1,4]]
        `shouldBe` [[3::Int,2],[1,4],[6,7,5]]
      it "e2" $ 
        ordenadosPorMaximo3 ["este","es","el","primero"]
        `shouldBe` ["el","primero","es","este"]
