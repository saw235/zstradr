-- | Main backtesting runner — drives the event-driven simulation loop.
module ZStradr.Backtest.Runner
  ( BacktestConfig (..)
  , BacktestResult (..)
  , runBacktest
  , totalEquity
  ) where

import ZStradr.Core.Types
import ZStradr.Backtest.Execution  (ExecutionConfig, fillOrder)
import ZStradr.Backtest.Portfolio  (emptyPortfolio, applyFill, updateUnrealizedPnl,
                                    totalEquity)
import ZStradr.Backtest.Strategy   (Strategy (..))

-- ---------------------------------------------------------------------------
-- Configuration and results

-- | Top-level configuration for a backtest run.
data BacktestConfig = BacktestConfig
  { startingCash    :: !Double
  , executionConfig :: !ExecutionConfig
  } deriving (Show, Eq)

-- | Summary results produced at the end of a backtest run.
data BacktestResult = BacktestResult
  { equityCurve    :: ![(Timestamp, Double)]
    -- ^ Total portfolio equity sampled once per bar.
  , trades         :: ![Trade]
    -- ^ All completed round-trip trades.
  , finalPortfolio :: !Portfolio
    -- ^ Portfolio state after the last bar.
  , totalReturn    :: !Double
    -- ^ @(finalEquity - initialCash) / initialCash@.
  , sharpeRatio    :: !Double
    -- ^ Annualised Sharpe ratio (assuming 252 trading days per year).
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Runner

-- | Run a backtest of strategy @s@ over a sorted list of 'OHLCV' bars.
--
-- Algorithm (per bar @i@):
--
--  1. @updateUnrealizedPnl@ — mark open positions to bar @i@'s close.
--  2. @onBar@ — ask the strategy for orders based on bar @i@.
--  3. For each order, if bar @i+1@ exists, attempt to fill at bar @i+1@.
--  4. @applyFill@ — update portfolio cash and positions.
--  5. Record @(bar[i].timestamp, totalEquity)@ in the equity curve.
runBacktest :: Strategy s => s -> BacktestConfig -> [OHLCV] -> BacktestResult
runBacktest strategy cfg bars =
  let initPortfolio  = emptyPortfolio (startingCash cfg)
      execCfg        = executionConfig cfg
      initCash       = startingCash cfg

      -- Zip each bar with its successor (Nothing for the last bar)
      barsWithNext   = zipWith (\b mb -> (b, mb)) bars
                                (map Just (tail bars) ++ [Nothing])

      -- Fold over bars accumulating (portfolio, trades, equityCurve)
      (finalP, allTrades, curve) =
        foldl (stepBar strategy execCfg)
              (initPortfolio, [], [])
              barsWithNext

      -- Compute performance metrics
      finalEq  = totalEquity finalP
      tReturn  = if initCash == 0 then 0 else (finalEq - initCash) / initCash
      sharpe   = computeSharpe curve

  in  BacktestResult
        { equityCurve    = reverse curve
        , trades         = reverse allTrades
        , finalPortfolio = finalP
        , totalReturn    = tReturn
        , sharpeRatio    = sharpe
        }

-- ---------------------------------------------------------------------------
-- Internal helpers

-- | Process a single bar in the simulation loop.
stepBar
  :: Strategy s
  => s
  -> ExecutionConfig
  -> (Portfolio, [Trade], [(Timestamp, Double)])  -- accumulator
  -> (OHLCV, Maybe OHLCV)                          -- (current bar, next bar)
  -> (Portfolio, [Trade], [(Timestamp, Double)])
stepBar strategy execCfg (portfolio, tradesAcc, curveAcc) (bar, mNextBar) =
  -- 1. Mark positions to current close
  let p1        = updateUnrealizedPnl portfolio bar

  -- 2. Ask strategy for orders
      orders    = onBar strategy p1 bar

  -- 3 & 4. Fill orders against the next bar (if it exists)
      (p2, newTrades) = case mNextBar of
        Nothing  -> (p1, [])
        Just nb  -> processFills execCfg p1 orders nb

  -- 5. Record equity
      eq        = totalEquity p2
      newCurve  = (ohlcvTimestamp bar, eq) : curveAcc
      newTrades' = newTrades ++ tradesAcc

  in  (p2, newTrades', newCurve)

-- | Attempt to fill each order against the next bar, accumulating fills.
processFills
  :: ExecutionConfig
  -> Portfolio
  -> [Order]
  -> OHLCV
  -> (Portfolio, [Trade])
processFills execCfg portfolio orders nextBar =
  foldl go (portfolio, []) orders
  where
    go (p, ts) order =
      case fillOrder execCfg order nextBar of
        Nothing   -> (p, ts)
        Just fill ->
          let (p', mTrade) = applyFill execCfg p fill
          in  (p', maybe ts (: ts) mTrade)

-- ---------------------------------------------------------------------------
-- Sharpe ratio

-- | Compute the annualised Sharpe ratio from the equity curve.
--
-- @sharpe = mean(dailyReturns) / stdDev(dailyReturns) * sqrt(252)@
--
-- Returns 0 when there are fewer than two data points or zero variance.
computeSharpe :: [(Timestamp, Double)] -> Double
computeSharpe curve
  | length equities < 2 = 0.0
  | variance == 0.0     = 0.0
  | otherwise           = (meanR / stdDev) * sqrt 252
  where
    -- curve is accumulated in reverse; equities are in correct order
    equities  = map snd (reverse curve)
    returns   = zipWith (\a b -> (b - a) / a) equities (tail equities)
    n         = fromIntegral (length returns) :: Double
    meanR     = sum returns / n
    variance  = sum (map (\r -> (r - meanR) ^ (2 :: Int)) returns) / n
    stdDev    = sqrt variance
