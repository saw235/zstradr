{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Tests for the backtesting runner using a synthetic SPY data set.
module ZStradr.Backtest.RunnerSpec (spec) where

import qualified Data.Map.Strict as Map
import           Test.Hspec

import ZStradr.Core.Types
import ZStradr.Backtest.Execution             (defaultConfig)
import ZStradr.Backtest.Runner                ( BacktestConfig (..)
                                              , BacktestResult (..)
                                              , runBacktest )
import ZStradr.Backtest.Strategies.BuyAndHold (BuyAndHold (..))

-- ---------------------------------------------------------------------------
-- Synthetic data

spy :: Symbol
spy = Symbol "SPY"

-- | 10 bars with gently rising prices.
--
--   Bar i: open = 100+i, high = 101+i, low = 99.5+i, close = 100.5+i
syntheticBars :: [OHLCV]
syntheticBars =
  [ OHLCV
      { ohlcvSymbol    = spy
      , ohlcvTimestamp = Timestamp (fromIntegral i * 86400000)  -- one day apart
      , ohlcvOpen      = 100.0 + fromIntegral i
      , ohlcvHigh      = 101.0 + fromIntegral i
      , ohlcvLow       =  99.5 + fromIntegral i
      , ohlcvClose     = 100.5 + fromIntegral i
      , ohlcvVolume    = 1_000_000
      }
  | i <- [0 .. 9 :: Int]
  ]

-- ---------------------------------------------------------------------------
-- Config and strategy

cfg :: BacktestConfig
cfg = BacktestConfig
  { startingCash    = 100_000.0
  , executionConfig = defaultConfig   -- 0 slippage, 0 commission
  }

strategy :: BuyAndHold
strategy = BuyAndHold { targetSymbol = spy, qty = 100.0 }

-- ---------------------------------------------------------------------------
-- Pre-compute result once for all tests
result :: BacktestResult
result = runBacktest strategy cfg syntheticBars

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "runBacktest (BuyAndHold / synthetic SPY)" $ do

  it "equity curve has exactly 10 entries (one per bar)" $
    length (equityCurve result) `shouldBe` 10

  it "final portfolio holds exactly 1 open position" $
    Map.size (portfolioPositions (finalPortfolio result))
      `shouldBe` 1

  it "final portfolio holds a SPY position" $
    Map.member spy (portfolioPositions (finalPortfolio result))
      `shouldBe` True

  it "total return is positive (prices rose over 10 bars)" $
    -- BuyAndHold buys at bar-1 open (101), last close is 109.5 → pnl positive
    totalReturn result `shouldSatisfy` (> 0)

  it "first equity equals starting cash (fill at cost basis, no price move yet)" $ do
    -- Equity on bar-0 is recorded after fills are applied.
    -- We fill at bar-1 open (= cost basis), so equity = startingCash.
    let firstEq = snd (head (equityCurve result))
    firstEq `shouldSatisfy` (\e -> abs (e - 100_000.0) < 0.01)

  it "sharpe ratio is a finite number" $
    let sr = sharpeRatio result
    in (not (isNaN sr) && not (isInfinite sr)) `shouldBe` True
