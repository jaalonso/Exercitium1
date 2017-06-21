module Primos_consecutivos_con_media_capicua_Spec (main, spec) where

import Test.Hspec
import Primos_consecutivos_con_media_capicua

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "primosConsecutivosConMediaCapicua" $ do
    describe "primosConsecutivosConMediaCapicua" $ do
      it "e1" $ 
        take 5 primosConsecutivosConMediaCapicua
        `shouldBe` [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
    describe "primosConsecutivosConMediaCapicua2" $ do
      it "e1" $ 
        take 5 primosConsecutivosConMediaCapicua
        `shouldBe` [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
