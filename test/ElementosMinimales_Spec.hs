module ElementosMinimales_Spec (main, spec) where

import Test.Hspec
import ElementosMinimales

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "minimales" $ do
    it "e1" $ 
      minimales [[1,3],[2,3,1],[3,2,5]]
      `shouldBe` [[2::Int,3,1],[3,2,5]]
    it "e2" $ 
      minimales [[1,3],[2,3,1],[3,2,5],[3,1]]
      `shouldBe` [[2::Int,3,1],[3,2,5]]
