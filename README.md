# ZStradr

**ZStradr** is a Haskell algorithmic trading strategy framework.

## Overview

ZStradr provides a strongly-typed, composable foundation for building,
backtesting, and deploying algorithmic trading strategies. It leverages
Haskell's type system to make strategy logic explicit and correct by
construction.

## Packages

| Package | Description |
|---------|-------------|
| `zstradr-core` | Domain types, interfaces, and shared utilities |
| `zstradr-backtest` | Event-driven backtesting simulation engine |
| `zstradr-cli` | Command-line interface for running strategies and backtests |

## Getting Started

### Prerequisites

- GHC 9.6+ (tested on 9.6.4 and 9.8.2)
- Cabal 3.10+

### Build

```bash
cabal update
cabal build all
```

### Test

```bash
cabal test all --test-show-details=always
```

### Run the CLI

```bash
cabal run zstradr
```

## Architecture

```
zstradr-core        # Pure domain: symbols, orders, fills, portfolio
zstradr-backtest    # Simulation engine: feeds bars through strategies
zstradr-cli         # User-facing CLI, configuration, entrypoint
```

## License

MIT
