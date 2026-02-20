-- | Typeclass for backtestable trading strategies.
module ZStradr.Backtest.Strategy
  ( Strategy (..)
  ) where

import ZStradr.Core.Types

-- | Any strategy that can be driven bar-by-bar through the backtesting loop.
class Strategy s where
  -- | Given the current portfolio and the latest completed OHLCV bar, return
  --   zero or more orders to submit for the next bar's open.
  onBar :: s -> Portfolio -> OHLCV -> [Order]
