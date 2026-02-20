module ZStradr.Core.TypesSpec (spec) where

import           Data.Aeson           (FromJSON, ToJSON, decode, encode)
import qualified Data.Map.Strict      as Map
import           Data.Time            (UTCTime (..))
import           Data.Time.Calendar   (fromGregorian)
import           Test.Hspec

import           ZStradr.Core.Types

-- ---------------------------------------------------------------------------
-- Helpers

sampleTime :: UTCTime
sampleTime = UTCTime (fromGregorian 2024 1 15) 0

sampleSymbol :: Symbol
sampleSymbol = Symbol "AAPL"

sampleOHLCV :: OHLCV
sampleOHLCV = OHLCV
  { ohlcvSymbol    = sampleSymbol
  , ohlcvTimestamp = sampleTime
  , ohlcvOpen      = 185.0
  , ohlcvHigh      = 188.5
  , ohlcvLow       = 184.0
  , ohlcvClose     = 187.3
  , ohlcvVolume    = 1_000_000
  }

sampleOrder :: Order
sampleOrder = Order
  { orderId        = OrderId "ord-001"
  , orderSymbol    = sampleSymbol
  , orderSide      = Buy
  , orderQty       = 10.0
  , orderType      = Limit 185.0
  , orderStatus    = Pending
  , orderTimestamp = sampleTime
  }

sampleFill :: Fill
sampleFill = Fill
  { fillOrderId    = OrderId "ord-001"
  , fillPrice      = 185.0
  , fillQty        = 10.0
  , fillTimestamp  = sampleTime
  , fillCommission = 1.0
  }

samplePosition :: Position
samplePosition = Position
  { posSymbol  = sampleSymbol
  , posQty     = 10.0
  , posAvgCost = 185.0
  }

samplePortfolio :: Portfolio
samplePortfolio = Portfolio
  { portCash        = 90_000.0
  , portPositions   = Map.singleton sampleSymbol samplePosition
  , portRealizedPnL = 250.0
  }

sampleExitFill :: Fill
sampleExitFill = Fill
  { fillOrderId    = OrderId "ord-002"
  , fillPrice      = 190.0
  , fillQty        = 10.0
  , fillTimestamp  = sampleTime
  , fillCommission = 1.0
  }

sampleTrade :: Trade
sampleTrade = Trade
  { tradeSymbol      = sampleSymbol
  , tradeEntryFill   = sampleFill
  , tradeExitFill    = sampleExitFill
  , tradeRealizedPnL = 49.0
  }

-- | JSON roundtrip helper.
roundtrip :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Bool
roundtrip x = decode (encode x) == Just x

-- ---------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = do
  describe "JSON roundtrip" $ do
    it "OHLCV" $
      roundtrip sampleOHLCV `shouldBe` True

    it "Order" $
      roundtrip sampleOrder `shouldBe` True

    it "Fill" $
      roundtrip sampleFill `shouldBe` True

    it "Portfolio" $
      roundtrip samplePortfolio `shouldBe` True

    it "Trade" $
      roundtrip sampleTrade `shouldBe` True

  describe "unrealizedPnL" $ do
    it "profit when price rises" $
      unrealizedPnL 190.0 samplePosition `shouldBe` 50.0

    it "loss when price falls" $
      unrealizedPnL 180.0 samplePosition `shouldBe` (-50.0)

    it "zero when price equals avg cost" $
      unrealizedPnL 185.0 samplePosition `shouldBe` 0.0

  describe "emptyPortfolio" $ do
    it "sets cash correctly" $
      portCash (emptyPortfolio 100_000) `shouldBe` 100_000

    it "has no positions" $
      portPositions (emptyPortfolio 100_000) `shouldBe` Map.empty

    it "has zero realized PnL" $
      portRealizedPnL (emptyPortfolio 100_000) `shouldBe` 0.0
