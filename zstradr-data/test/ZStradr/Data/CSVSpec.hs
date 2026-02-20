module ZStradr.Data.CSVSpec (spec) where

import           System.IO.Temp       (withSystemTempFile)
import           System.IO            (hPutStr, hClose)
import           Test.Hspec

import           ZStradr.Core.Types   (Symbol (..), ohlcvOpen, ohlcvHigh,
                                        ohlcvLow, ohlcvClose, ohlcvVolume)
import           ZStradr.Data.CSV     (loadCSV)
import           ZStradr.Data.MarketData (MarketDataError (..))

-- ---------------------------------------------------------------------------
-- Sample CSV data

sampleCSV :: String
sampleCSV = unlines
  [ "date,open,high,low,close,volume"
  , "2024-01-15,185.00,188.50,184.00,187.30,1000000"
  , "2024-01-16,187.30,191.20,186.50,190.50,1200000"
  , "2024-01-17,190.50,192.00,189.00,191.80,900000"
  ]

negativePriceCSV :: String
negativePriceCSV = unlines
  [ "date,open,high,low,close,volume"
  , "2024-01-15,-185.00,188.50,184.00,187.30,1000000"
  ]

zeroVolumeCSV :: String
zeroVolumeCSV = unlines
  [ "date,open,high,low,close,volume"
  , "2024-01-15,185.00,188.50,184.00,187.30,0"
  ]

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "CSV" $ do

  it "parses 3 rows of AAPL data correctly" $
    withSystemTempFile "aapl.csv" $ \path h -> do
      hPutStr h sampleCSV
      hClose h
      result <- loadCSV (Symbol "AAPL") path
      case result of
        Left err   -> expectationFailure ("Unexpected error: " <> show err)
        Right bars -> do
          length bars `shouldBe` 3
          -- First bar values
          let bar0 = head bars
          ohlcvOpen  bar0 `shouldBe` 185.00
          ohlcvHigh  bar0 `shouldBe` 188.50
          ohlcvLow   bar0 `shouldBe` 184.00
          ohlcvClose bar0 `shouldBe` 187.30
          ohlcvVolume bar0 `shouldBe` 1000000

  it "returns Left (NegativePrice _) for negative open price" $
    withSystemTempFile "neg.csv" $ \path h -> do
      hPutStr h negativePriceCSV
      hClose h
      result <- loadCSV (Symbol "AAPL") path
      case result of
        Left (NegativePrice _) -> return ()
        other -> expectationFailure ("Expected NegativePrice, got: " <> show other)

  it "returns Left ZeroVolume for zero volume" $
    withSystemTempFile "zero.csv" $ \path h -> do
      hPutStr h zeroVolumeCSV
      hClose h
      result <- loadCSV (Symbol "AAPL") path
      result `shouldBe` Left ZeroVolume
