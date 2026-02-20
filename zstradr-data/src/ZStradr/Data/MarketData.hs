module ZStradr.Data.MarketData
  ( -- * Date range
    DateRange(..)
    -- * Errors
  , MarketDataError(..)
    -- * Validation
  , validateOHLCV
  ) where

import Data.Time       (UTCTime)
import ZStradr.Core.Types (OHLCV(..))

-- ---------------------------------------------------------------------------
-- DateRange

-- | A closed date range [start, end] for market data queries.
data DateRange = DateRange
  { drStart :: UTCTime
  , drEnd   :: UTCTime
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- MarketDataError

data MarketDataError
  = MissingData  String
  | NetworkError String
  | ParseError   String
  | NegativePrice Double
  | ZeroVolume
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Validation

-- | Validate a list of OHLCV bars, returning the first error found or the
-- original list if all bars pass.
validateOHLCV :: [OHLCV] -> Either MarketDataError [OHLCV]
validateOHLCV = mapM validateOne
  where
    validateOne bar
      | ohlcvOpen  bar < 0 = Left (NegativePrice (ohlcvOpen  bar))
      | ohlcvHigh  bar < 0 = Left (NegativePrice (ohlcvHigh  bar))
      | ohlcvLow   bar < 0 = Left (NegativePrice (ohlcvLow   bar))
      | ohlcvClose bar < 0 = Left (NegativePrice (ohlcvClose bar))
      | ohlcvVolume bar == 0 = Left ZeroVolume
      | otherwise            = Right bar
