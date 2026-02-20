{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ZStradr.Core.Types
  ( -- * Symbol
    Symbol(..)
    -- * Side
  , Side(..)
    -- * OHLCV bar
  , OHLCV(..)
    -- * Orders
  , OrderType(..)
  , OrderStatus(..)
  , OrderId(..)
  , Order(..)
    -- * Fill
  , Fill(..)
    -- * Position
  , Position(..)
  , unrealizedPnL
    -- * Portfolio
  , Portfolio(..)
  , emptyPortfolio
  ) where

import           Data.Aeson      (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import           Data.Map.Strict (Map)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics    (Generic)

-- ---------------------------------------------------------------------------
-- Symbol

newtype Symbol = Symbol { unSymbol :: Text }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving newtype  (FromJSONKey, ToJSONKey)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Side

data Side = Buy | Sell
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- OHLCV bar

data OHLCV = OHLCV
  { ohlcvSymbol    :: Symbol
  , ohlcvTimestamp :: UTCTime
  , ohlcvOpen      :: Double
  , ohlcvHigh      :: Double
  , ohlcvLow       :: Double
  , ohlcvClose     :: Double
  , ohlcvVolume    :: Double
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Order types

data OrderType
  = Market
  | Limit Double
  | StopLimit Double Double
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data OrderStatus
  = Pending
  | Filled
  | PartiallyFilled
  | Cancelled
  | Rejected
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype OrderId = OrderId { unOrderId :: Text }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Order = Order
  { orderId        :: OrderId
  , orderSymbol    :: Symbol
  , orderSide      :: Side
  , orderQty       :: Double
  , orderType      :: OrderType
  , orderStatus    :: OrderStatus
  , orderTimestamp :: UTCTime
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Fill (execution record)

data Fill = Fill
  { fillOrderId    :: OrderId
  , fillPrice      :: Double
  , fillQty        :: Double
  , fillTimestamp  :: UTCTime
  , fillCommission :: Double
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Position

data Position = Position
  { posSymbol  :: Symbol
  , posQty     :: Double
  , posAvgCost :: Double
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Compute unrealized PnL given a current market price.
unrealizedPnL :: Double -> Position -> Double
unrealizedPnL currentPrice pos =
  (currentPrice - posAvgCost pos) * posQty pos

-- ---------------------------------------------------------------------------
-- Portfolio

data Portfolio = Portfolio
  { portCash        :: Double
  , portPositions   :: Map Symbol Position
  , portRealizedPnL :: Double
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Create an empty portfolio with the given initial cash balance.
emptyPortfolio :: Double -> Portfolio
emptyPortfolio initialCash = Portfolio initialCash mempty 0.0
