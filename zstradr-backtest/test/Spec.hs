module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ZStradr.Backtest.Engine" $ do
    it "placeholder - engine tests to be added" $
      True `shouldBe` True
