module Anagramas_Spec (main, spec) where

import Anagramas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "anagramas" $ do
    describe "anagramas" $ do
      it "e1" $ 
        anagramas "amor" ["Roma","mola","loma","moRa", "rama"] 
        `shouldBe` ["Roma","moRa"]
      it "e2" $ 
        anagramas "rama" ["aMar","amaRa","roMa","marr","aRma"]
        `shouldBe` ["aMar","aRma"]
    describe "sonAnagramas2" $ do
      it "e1" $
        sonAnagramas2 "amor" "Roma"  `shouldBe`  True
      it "e2" $
        sonAnagramas2 "amor" "mola"  `shouldBe`  False
    describe "sonAnagramas3" $ do
      it "e1" $
        sonAnagramas3 "amor" "Roma"  `shouldBe`  True
      it "e2" $
        sonAnagramas3 "amor" "mola"  `shouldBe`  False
      
    describe "anagramas2" $ do
      it "e1" $ 
        anagramas2 "amor" ["Roma","mola","loma","moRa", "rama"] 
        `shouldBe` ["Roma","moRa"]
      it "e2" $ 
        anagramas2 "rama" ["aMar","amaRa","roMa","marr","aRma"]
        `shouldBe` ["aMar","aRma"]
    describe "anagramas3" $ do
      it "e1" $ 
        anagramas3 "amor" ["Roma","mola","loma","moRa", "rama"] 
        `shouldBe` ["Roma","moRa"]
      it "e2" $ 
        anagramas3 "rama" ["aMar","amaRa","roMa","marr","aRma"]
        `shouldBe` ["aMar","aRma"]
