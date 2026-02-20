{-# LANGUAGE OverloadedStrings #-}
-- | A simple buy-and-hold strategy: buy @qty@ shares of @targetSymbol@ on
--   the first bar and never sell.
module ZStradr.Backtest.Strategies.BuyAndHold
  ( BuyAndHold (..)
  ) where

import ZStradr.Core.Types
import ZStradr.Backtest.Strategy  (Strategy (..))
import ZStradr.Backtest.Portfolio (hasPosition)

-- | Strategy parameters.
data BuyAndHold = BuyAndHold
  { targetSymbol :: !Symbol
    -- ^ The symbol to buy and hold.
  , qty          :: !Double
    -- ^ Number of shares to purchase on the first bar.
  } deriving (Show, Eq)

instance Strategy BuyAndHold where
  onBar s portfolio bar
    | ohlcvSymbol bar == targetSymbol s
    , not (hasPosition portfolio (targetSymbol s))
    = [marketBuyOrder s]
    | otherwise
    = []

-- | Create a pending market-buy order for the strategy's target symbol.
marketBuyOrder :: BuyAndHold -> Order
marketBuyOrder s = Order
  { orderId     = "buy-" <> unSymbol (targetSymbol s)
  , orderSymbol = targetSymbol s
  , orderSide   = Buy
  , orderType   = Market
  , orderQty    = qty s
  , orderStatus = Pending
  }
