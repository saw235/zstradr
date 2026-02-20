# ZStradr

A modular, type-safe quantitative trading framework written in Haskell.

## Overview

ZStradr is built as a Cabal multi-package project. Each package has a single,
well-defined responsibility:

| Package | Description |
|---|---|
| `zstradr-core` | Core domain types: OHLCV bars, orders, positions, portfolio, trades |
| `zstradr-data` | Market data ingestion: CSV loader and Yahoo Finance adapter |
| `zstradr-backtest` | Event-driven backtesting engine and strategy interface |
| `zstradr-cli` | Command-line interface for running backtests and fetching data |

## Prerequisites

- [GHC 9.6+](https://www.haskell.org/ghc/)
- [Cabal 3.10+](https://www.haskell.org/cabal/)

The easiest way to install both is via [GHCup](https://www.haskell.org/ghcup/):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6
ghcup install cabal latest
ghcup set ghc 9.6
```

## Building

```bash
# Clone the repo
git clone https://github.com/saw235/zstradr.git
cd zstradr

# Update package index
cabal update

# Build all packages
cabal build all

# Run all tests
cabal test all

# Build and run the CLI
cabal run zstradr -- version
```

## Package Details

### `zstradr-core`

Fundamental domain types shared by all other packages.

- `ZStradr.Core.Types` — `Symbol`, `Timestamp`, `OHLCV`, `Order`, `Fill`, `Position`, `Portfolio`, `Trade`
- All types derive `Generic`, `NFData`, `FromJSON`, `ToJSON` for zero-boilerplate serialisation and strict evaluation.

### `zstradr-data`

Market data ingestion layer.

- `ZStradr.Data.CSV` — Load OHLCV bars from a standard `date,open,high,low,close,volume` CSV file. Validates prices and volume.
- `ZStradr.Data.Yahoo` — Fetch historical bars from the Yahoo Finance v8 chart API.
- `ZStradr.Data.MarketData` — In-memory `MarketDataStore` (a `Map Symbol (Vector OHLCV)`).

### `zstradr-backtest`

Event-driven backtesting engine (under development).

### `zstradr-cli`

Command-line interface (under development).

## Project Structure

```
zstradr/
├── cabal.project
├── .github/workflows/ci.yml
├── zstradr-core/
│   ├── zstradr-core.cabal
│   ├── src/ZStradr/Core/Types.hs
│   └── test/
├── zstradr-data/
│   ├── zstradr-data.cabal
│   ├── src/ZStradr/Data/
│   │   ├── CSV.hs
│   │   ├── MarketData.hs
│   │   └── Yahoo.hs
│   └── test/
├── zstradr-backtest/
│   ├── zstradr-backtest.cabal
│   └── src/ZStradr/Backtest/Engine.hs
└── zstradr-cli/
    ├── zstradr-cli.cabal
    └── app/Main.hs
```

## CI

GitHub Actions runs `cabal build all` and `cabal test all` on every push to
`main` and on every pull request. See `.github/workflows/ci.yml`.

## License

MIT — see [LICENSE](LICENSE) for details.
