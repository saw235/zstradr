{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | In-memory market data store.
--
-- A 'MarketDataStore' is an immutable snapshot of OHLCV bars for one or more
-- instruments, indexed by 'Symbol' and kept sorted by 'Timestamp'.
module ZStradr.Data.MarketData
  ( MarketDataStore
  , empty
  , loadCSVStore
  , insertBars
  , lookupSymbol
  , symbolData
  , symbols
  , totalBars
  ) where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Vector        (Vector)
import qualified Data.Vector        as V
import qualified Data.Vector.Algorithms.Intro as VA

import           ZStradr.Core.Types  (OHLCV (..), Symbol, Timestamp (..))
import           ZStradr.Data.CSV    (CSVError, loadCSV)

-- ---------------------------------------------------------------------------
-- Type

-- | An in-memory store mapping each 'Symbol' to a time-sorted 'Vector' of
-- 'OHLCV' bars.
newtype MarketDataStore = MarketDataStore
  { _storeMap :: Map Symbol (Vector OHLCV)
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Construction

-- | An empty store with no symbols.
empty :: MarketDataStore
empty = MarketDataStore Map.empty

-- | Load OHLCV bars for one symbol from a CSV file and add them to the store.
--
-- The bars are sorted by timestamp after loading.  If the symbol already
-- exists in the store the new bars are merged with the existing ones and
-- re-sorted.
loadCSVStore
  :: Symbol
  -> FilePath
  -> MarketDataStore
  -> IO (Either CSVError MarketDataStore)
loadCSVStore sym path store = do
  result <- loadCSV sym path
  case result of
    Left  err  -> return (Left err)
    Right bars -> return (Right (insertBars sym bars store))

-- | Insert a 'Vector' of bars for a symbol, sorting by timestamp.
--
-- Existing bars for the same symbol are merged (union by timestamp — existing
-- bars are kept on collision).
insertBars :: Symbol -> Vector OHLCV -> MarketDataStore -> MarketDataStore
insertBars sym newBars (MarketDataStore m) =
  let merged = case Map.lookup sym m of
                 Nothing       -> newBars
                 Just existing -> sortByTimestamp (existing <> newBars)
  in  MarketDataStore (Map.insert sym merged m)

-- ---------------------------------------------------------------------------
-- Queries

-- | Look up all bars for a symbol. Returns 'Nothing' if the symbol is absent.
lookupSymbol :: Symbol -> MarketDataStore -> Maybe (Vector OHLCV)
lookupSymbol sym (MarketDataStore m) = Map.lookup sym m

-- | Like 'lookupSymbol' but returns an empty vector instead of 'Nothing'.
symbolData :: Symbol -> MarketDataStore -> Vector OHLCV
symbolData sym store =
  case lookupSymbol sym store of
    Nothing   -> V.empty
    Just bars -> bars

-- | All symbols present in the store.
symbols :: MarketDataStore -> [Symbol]
symbols (MarketDataStore m) = Map.keys m

-- | Total number of bars across all symbols.
totalBars :: MarketDataStore -> Int
totalBars (MarketDataStore m) = Map.foldl' (\acc v -> acc + V.length v) 0 m

-- ---------------------------------------------------------------------------
-- Internal helpers

-- | Sort a vector of OHLCV bars by ascending timestamp in-place (via the
-- 'vector-algorithms' intro-sort).
sortByTimestamp :: Vector OHLCV -> Vector OHLCV
sortByTimestamp v = V.modify sortMut v
  where
    sortMut mv = VA.sortBy cmpTimestamp mv
    cmpTimestamp a b =
      compare (unTimestamp (ohlcvTimestamp a))
              (unTimestamp (ohlcvTimestamp b))
