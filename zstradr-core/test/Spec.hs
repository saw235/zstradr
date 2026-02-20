module Main (main) where

import Test.Hspec
import qualified ZStradr.Core.TypesSpec as TypesSpec

main :: IO ()
main = hspec TypesSpec.spec
