-- | ZStradr backtesting simulation engine.
--
-- This module will house the event-driven backtesting loop.
-- Currently a scaffold placeholder.
module ZStradr.Backtest.Engine
  ( runBacktest
  ) where

import ZStradr.Core.Types (Portfolio, OHLCV)

-- | Run a backtest over a series of OHLCV bars, starting from an initial
-- portfolio. Returns the final portfolio.
--
-- This is a placeholder implementation; the full event-driven loop
-- will be fleshed out in subsequent issues.
runBacktest :: Portfolio -> [OHLCV] -> Portfolio
runBacktest portfolio _ = portfolio
