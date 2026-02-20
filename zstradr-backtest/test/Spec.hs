module Main (main) where

import Test.Hspec
import qualified ZStradr.Backtest.RunnerSpec as RunnerSpec

main :: IO ()
main = hspec $ do
  RunnerSpec.spec
