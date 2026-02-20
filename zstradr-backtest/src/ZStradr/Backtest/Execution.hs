-- | Order execution simulator for the backtesting engine.
--
-- Market orders are filled at the next bar's open price (plus slippage).
-- Limit orders are filled when the open price satisfies the limit condition.
module ZStradr.Backtest.Execution
  ( ExecutionConfig (..)
  , defaultConfig
  , fillOrder
  ) where

import ZStradr.Core.Types

-- | Configuration for the simulated execution engine.
data ExecutionConfig = ExecutionConfig
  { slippagePct    :: !Double
    -- ^ Fractional slippage applied to the fill price (e.g. 0.001 = 0.1 %).
  , commissionFixed :: !Double
    -- ^ Fixed per-trade commission in currency units (e.g. 1.0 = $1).
  } deriving (Show, Eq)

-- | No slippage, no commission — useful for idealized simulations.
defaultConfig :: ExecutionConfig
defaultConfig = ExecutionConfig 0.0 0.0

-- | Attempt to fill an 'Order' using the OHLCV bar that follows the signal bar.
--
-- * 'Market' orders always fill at @open * (1 ± slippage)@.
-- * 'Limit' orders fill only when the open price satisfies the limit:
--     - Buy:  @open <= limitPrice@
--     - Sell: @open >= limitPrice@
-- * 'StopLimit' orders are not yet supported and return 'Nothing'.
--
-- Returns 'Nothing' when the order cannot be filled on this bar.
fillOrder :: ExecutionConfig -> Order -> OHLCV -> Maybe Fill
fillOrder cfg order nextBar =
  case orderType order of
    Market        -> Just (mkFill (applySlippage (orderSide order) (ohlcvOpen nextBar)))
    Limit lp      -> fillLimit lp
    StopLimit {}  -> Nothing   -- not implemented
  where
    side      = orderSide order
    openPrice = ohlcvOpen nextBar
    ts        = ohlcvTimestamp nextBar

    applySlippage Buy  p = p * (1.0 + slippagePct cfg)
    applySlippage Sell p = p * (1.0 - slippagePct cfg)

    fillLimit lp =
      case side of
        Buy  | openPrice <= lp -> Just (mkFill (applySlippage Buy  openPrice))
        Sell | openPrice >= lp -> Just (mkFill (applySlippage Sell openPrice))
        _                      -> Nothing

    mkFill price = Fill
      { fillOrderId   = orderId order
      , fillSymbol    = orderSymbol order
      , fillSide      = side
      , fillQty       = orderQty order
      , fillPrice     = price
      , fillTimestamp = ts
      }
