-- | Event types for the event-driven backtesting loop.
module ZStradr.Backtest.Events
  ( Event (..)
  ) where

import ZStradr.Core.Types

-- | The sum of all events that can flow through the simulation queue.
data Event
  = MarketEvent OHLCV
    -- ^ A new price bar has arrived.
  | SignalEvent
      { signalSymbol   :: !Symbol
      , signalStrength :: !Double
      }
    -- ^ A strategy has generated a directional signal.
  | OrderEvent Order
    -- ^ An order has been submitted for execution.
  | FillEvent Fill
    -- ^ An order has been (fully) filled.
  deriving (Show, Eq)
