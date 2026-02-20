{-# LANGUAGE OverloadedStrings #-}

module ZStradr.Core.TypesSpec (spec) where

import           Data.Aeson        (decode, encode)
import           Test.Hspec

import           ZStradr.Core.Types

-- ---------------------------------------------------------------------------
-- Spec entry point

spec :: Spec
spec = do
  symbolSpec
  timestampSpec
  ohlcvSpec
  orderSideSpec
  orderTypeSpec
  orderStatusSpec

-- ---------------------------------------------------------------------------
-- Symbol

symbolSpec :: Spec
symbolSpec = describe "Symbol" $ do
  it "wraps and unwraps Text" $ do
    let s = Symbol "AAPL"
    unSymbol s `shouldBe` "AAPL"

  it "compares equal when same ticker" $ do
    Symbol "MSFT" `shouldBe` Symbol "MSFT"

  it "compares unequal for different tickers" $ do
    Symbol "AAPL" `shouldNotBe` Symbol "GOOG"

  it "JSON roundtrip" $ do
    let s = Symbol "TSLA"
    decode (encode s) `shouldBe` Just s

-- ---------------------------------------------------------------------------
-- Timestamp

timestampSpec :: Spec
timestampSpec = describe "Timestamp" $ do
  it "wraps and unwraps Integer" $ do
    let t = Timestamp 1_700_000_000_000
    unTimestamp t `shouldBe` 1_700_000_000_000

  it "JSON roundtrip" $ do
    let t = Timestamp 0
    decode (encode t) `shouldBe` Just t

-- ---------------------------------------------------------------------------
-- OHLCV

sampleOHLCV :: OHLCV
sampleOHLCV = OHLCV
  { ohlcvSymbol    = Symbol "AAPL"
  , ohlcvTimestamp = Timestamp 1_700_000_000_000
  , ohlcvOpen      = 150.0
  , ohlcvHigh      = 155.0
  , ohlcvLow       = 149.0
  , ohlcvClose     = 153.5
  , ohlcvVolume    = 1_000_000.0
  }

ohlcvSpec :: Spec
ohlcvSpec = describe "OHLCV" $ do
  it "stores correct field values" $ do
    ohlcvSymbol sampleOHLCV    `shouldBe` Symbol "AAPL"
    ohlcvOpen   sampleOHLCV    `shouldBe` 150.0
    ohlcvHigh   sampleOHLCV    `shouldBe` 155.0
    ohlcvLow    sampleOHLCV    `shouldBe` 149.0
    ohlcvClose  sampleOHLCV    `shouldBe` 153.5
    ohlcvVolume sampleOHLCV    `shouldBe` 1_000_000.0

  it "JSON roundtrip preserves all fields" $ do
    decode (encode sampleOHLCV) `shouldBe` Just sampleOHLCV

-- ---------------------------------------------------------------------------
-- OrderSide

orderSideSpec :: Spec
orderSideSpec = describe "OrderSide" $ do
  it "Show Buy" $ show Buy  `shouldBe` "Buy"
  it "Show Sell" $ show Sell `shouldBe` "Sell"

  it "Buy /= Sell" $ Buy `shouldNotBe` Sell

  it "JSON roundtrip Buy" $
    decode (encode Buy) `shouldBe` Just Buy

  it "JSON roundtrip Sell" $
    decode (encode Sell) `shouldBe` Just Sell

-- ---------------------------------------------------------------------------
-- OrderType

orderTypeSpec :: Spec
orderTypeSpec = describe "OrderType" $ do
  it "Market roundtrip" $
    decode (encode Market) `shouldBe` Just Market

  it "Limit roundtrip" $ do
    let t = Limit 200.50
    decode (encode t) `shouldBe` Just t

  it "StopLimit roundtrip" $ do
    let t = StopLimit { stopPrice = 195.0, limitPrice = 194.0 }
    decode (encode t) `shouldBe` Just t

-- ---------------------------------------------------------------------------
-- OrderStatus

orderStatusSpec :: Spec
orderStatusSpec = describe "OrderStatus" $ do
  let statuses = [Pending, PartiallyFilled, Filled, Cancelled, Rejected]
  mapM_ (\s -> it ("JSON roundtrip " <> show s) $
    decode (encode s) `shouldBe` Just s) statuses
