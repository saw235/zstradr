-- | Portfolio state machine for the backtesting engine.
module ZStradr.Backtest.Portfolio
  ( emptyPortfolio
  , applyFill
  , updateUnrealizedPnl
  , hasPosition
  , totalEquity
  ) where

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map

import ZStradr.Core.Types
import ZStradr.Backtest.Execution (ExecutionConfig (..))

-- ---------------------------------------------------------------------------
-- Construction

-- | Create an empty 'Portfolio' with the given starting cash balance.
emptyPortfolio :: Double -> Portfolio
emptyPortfolio cash = Portfolio
  { portfolioCash        = cash
  , portfolioPositions   = Map.empty
  , portfolioRealizedPnl = 0.0
  }

-- ---------------------------------------------------------------------------
-- State transitions

-- | Apply a 'Fill' to a 'Portfolio', returning the updated portfolio and
--   (when a position is fully closed) the completed 'Trade'.
--
--   * Buy fills reduce cash by @qty * price + commission@ and update
--     the open position (volume-weighted average cost).
--   * Sell fills increase cash by @qty * price - commission@, reduce the
--     position, and book realised PnL.  A 'Trade' is returned when the
--     position is completely closed.
applyFill :: ExecutionConfig -> Portfolio -> Fill -> (Portfolio, Maybe Trade)
applyFill cfg p fill =
  case fillSide fill of
    Buy  -> (applyBuy  p fill cfg, Nothing)
    Sell -> applySell p fill cfg

-- | Total portfolio equity: cash + mark-to-market value of all positions.
--
--   After 'updateUnrealizedPnl' has been called the market value is
--   @posQty * posAvgCost + posUnrealizedPnl@.  Before any update we fall back
--   to @posQty * posAvgCost@ (cost basis as proxy).
totalEquity :: Portfolio -> Double
totalEquity p =
  portfolioCash p
  + sum [ posQty pos * posAvgCost pos + posUnrealizedPnl pos
        | pos <- Map.elems (portfolioPositions p) ]

-- ---------------------------------------------------------------------------
-- Unrealised PnL

-- | Mark an open position to the close price of the given 'OHLCV' bar.
--   Positions in other symbols are unchanged.
updateUnrealizedPnl :: Portfolio -> OHLCV -> Portfolio
updateUnrealizedPnl p bar =
  let sym   = ohlcvSymbol bar
      price = ohlcvClose  bar
  in case Map.lookup sym (portfolioPositions p) of
       Nothing  -> p
       Just pos ->
         let newPnl = posQty pos * (price - posAvgCost pos)
             newPos = pos { posUnrealizedPnl = newPnl }
         in  p { portfolioPositions =
                   Map.insert sym newPos (portfolioPositions p) }

-- ---------------------------------------------------------------------------
-- Queries

-- | Returns 'True' when the portfolio holds an open position in the given symbol.
hasPosition :: Portfolio -> Symbol -> Bool
hasPosition p sym = Map.member sym (portfolioPositions p)

-- ---------------------------------------------------------------------------
-- Internal helpers

applyBuy :: Portfolio -> Fill -> ExecutionConfig -> Portfolio
applyBuy p fill cfg =
  let sym   = fillSymbol fill
      qty   = fillQty    fill
      price = fillPrice  fill
      cost  = qty * price + commissionFixed cfg
      newPos = case Map.lookup sym (portfolioPositions p) of
        Nothing ->
          Position
            { posSymbol        = sym
            , posQty           = qty
            , posAvgCost       = price
            , posUnrealizedPnl = 0.0
            }
        Just pos ->
          let totalQty = posQty pos + qty
              -- Volume-weighted average cost
              newAvg   = (posQty pos * posAvgCost pos + qty * price) / totalQty
          in  pos { posQty     = totalQty
                  , posAvgCost = newAvg
                  }
  in  p { portfolioCash      = portfolioCash p - cost
        , portfolioPositions = Map.insert sym newPos (portfolioPositions p)
        }

applySell :: Portfolio -> Fill -> ExecutionConfig -> (Portfolio, Maybe Trade)
applySell p fill cfg =
  case Map.lookup (fillSymbol fill) (portfolioPositions p) of
    Nothing ->
      -- Selling something we don't hold — ignore gracefully
      (p, Nothing)
    Just pos ->
      let sym          = fillSymbol fill
          qty          = fillQty    fill
          price        = fillPrice  fill
          proceeds     = qty * price - commissionFixed cfg
          pnlDelta     = (price - posAvgCost pos) * qty - commissionFixed cfg
          remainingQty = posQty pos - qty

          (newPositions, mTrade) =
            if remainingQty <= 0
              then
                -- Position fully closed → emit a Trade
                let trade = Trade
                      { tradeSymbol      = sym
                      , tradeEntrySide   = Buy
                      , tradeEntryPrice  = posAvgCost pos
                      , tradeEntryTime   = fillTimestamp fill  -- best approximation
                      , tradeExitPrice   = price
                      , tradeExitTime    = fillTimestamp fill
                      , tradeQty         = posQty pos
                      , tradePnl         = pnlDelta
                      }
                in  (Map.delete sym (portfolioPositions p), Just trade)
              else
                -- Position partially closed
                let updatedPos = pos
                      { posQty           = remainingQty
                      , posUnrealizedPnl = remainingQty * (price - posAvgCost pos)
                      }
                in  (Map.insert sym updatedPos (portfolioPositions p), Nothing)

          newPortfolio = p
            { portfolioCash        = portfolioCash p + proceeds
            , portfolioPositions   = newPositions
            , portfolioRealizedPnl = portfolioRealizedPnl p + pnlDelta
            }
      in  (newPortfolio, mTrade)
