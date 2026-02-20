module Main (main) where

import ZStradr.Backtest.Engine (runBacktest)
import ZStradr.Core.Types      (emptyPortfolio, portCash)

main :: IO ()
main = do
  putStrLn "ZStradr — Haskell Algorithmic Trading Framework"
  let portfolio = emptyPortfolio 100_000
  putStrLn $ "Starting portfolio cash: " <> show (portCash portfolio)
  let final = runBacktest portfolio []
  putStrLn $ "Final portfolio cash:    " <> show (portCash final)
