module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ZStradr.Data" $ do
    it "placeholder — CSV and Yahoo tests to be added" $
      True `shouldBe` True
