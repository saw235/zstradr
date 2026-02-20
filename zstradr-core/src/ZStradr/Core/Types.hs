{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core domain types for the ZStradr trading framework.
--
-- All types derive 'Generic', 'NFData', 'FromJSON', and 'ToJSON' so they can
-- be used with aeson-based serialisation and Control.DeepSeq evaluation
-- strategies without boilerplate.
module ZStradr.Core.Types
  ( -- * Primitives
    Symbol (..)
  , Timestamp (..)
    -- * Market data
  , OHLCV (..)
    -- * Orders
  , OrderSide (..)
  , OrderType (..)
  , OrderStatus (..)
  , Order (..)
  , Fill (..)
    -- * Portfolio
  , Position (..)
  , Portfolio (..)
    -- * Trades
  , Trade (..)
  ) where

import           Control.DeepSeq  (NFData)
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Map.Strict  (Map)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)

-- ---------------------------------------------------------------------------
-- Primitives

-- | A stock ticker symbol (e.g. @"AAPL"@, @"MSFT"@).
newtype Symbol = Symbol { unSymbol :: Text }
  deriving (Show, Eq, Ord, Generic, NFData, FromJSON, ToJSON)

-- | Unix epoch timestamp in milliseconds.
newtype Timestamp = Timestamp { unTimestamp :: Integer }
  deriving (Show, Eq, Ord, Generic, NFData, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Market data

-- | An OHLCV bar for a single instrument and time period.
data OHLCV = OHLCV
  { ohlcvSymbol    :: !Symbol
  , ohlcvTimestamp :: !Timestamp
  , ohlcvOpen      :: !Double
  , ohlcvHigh      :: !Double
  , ohlcvLow       :: !Double
  , ohlcvClose     :: !Double
  , ohlcvVolume    :: !Double
  } deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Orders

-- | Whether an order is a buy or sell.
data OrderSide = Buy | Sell
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, FromJSON, ToJSON)

-- | The execution type of an order.
data OrderType
  = Market
    -- ^ Execute immediately at the best available price.
  | Limit   !Double
    -- ^ Execute at the given limit price or better.
  | StopLimit
      { stopPrice  :: !Double
        -- ^ Trigger price that activates the limit order.
      , limitPrice :: !Double
        -- ^ Maximum (buy) or minimum (sell) execution price.
      }
  deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)

-- | Lifecycle state of an order.
data OrderStatus
  = Pending
  | PartiallyFilled
  | Filled
  | Cancelled
  | Rejected
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, NFData, FromJSON, ToJSON)

-- | A trading order.
data Order = Order
  { orderId     :: !Text
    -- ^ Unique order identifier.
  , orderSymbol :: !Symbol
  , orderSide   :: !OrderSide
  , orderType   :: !OrderType
  , orderQty    :: !Double
    -- ^ Number of shares / contracts.
  , orderStatus :: !OrderStatus
  } deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)

-- | A single execution (partial or complete fill) of an order.
data Fill = Fill
  { fillOrderId   :: !Text
  , fillSymbol    :: !Symbol
  , fillSide      :: !OrderSide
  , fillQty       :: !Double
  , fillPrice     :: !Double
  , fillTimestamp :: !Timestamp
  } deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Portfolio

-- | An open position in a single instrument.
data Position = Position
  { posSymbol        :: !Symbol
  , posQty           :: !Double
    -- ^ Signed quantity: positive = long, negative = short.
  , posAvgCost       :: !Double
    -- ^ Volume-weighted average entry price.
  , posUnrealizedPnl :: !Double
    -- ^ Mark-to-market unrealised profit / loss.
  } deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)

-- | The overall portfolio state.
data Portfolio = Portfolio
  { portfolioCash         :: !Double
    -- ^ Available cash balance.
  , portfolioPositions    :: !(Map Symbol Position)
    -- ^ Currently open positions, keyed by symbol.
  , portfolioRealizedPnl  :: !Double
    -- ^ Cumulative realised profit / loss.
  } deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Trades

-- | A completed round-trip trade (entry + exit).
data Trade = Trade
  { tradeSymbol      :: !Symbol
  , tradeEntrySide   :: !OrderSide
  , tradeEntryPrice  :: !Double
  , tradeEntryTime   :: !Timestamp
  , tradeExitPrice   :: !Double
  , tradeExitTime    :: !Timestamp
  , tradeQty         :: !Double
  , tradePnl         :: !Double
    -- ^ Realised profit / loss for this trade.
  } deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON)
