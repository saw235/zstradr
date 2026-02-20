module ZStradr.Data.CSV
  ( loadCSV
  ) where

import           Control.Exception        (IOException, try)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Csv                 as Csv
import           Data.Time                (UTCTime (..), secondsToDiffTime)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)
import qualified Data.Vector              as V

import           ZStradr.Core.Types       (OHLCV (..), Symbol)
import           ZStradr.Data.MarketData  (MarketDataError (..), validateOHLCV)

-- ---------------------------------------------------------------------------
-- Internal row type

data OHLCVRow = OHLCVRow
  { rowDate   :: !String
  , rowOpen   :: !Double
  , rowHigh   :: !Double
  , rowLow    :: !Double
  , rowClose  :: !Double
  , rowVolume :: !Double
  }

instance Csv.FromNamedRecord OHLCVRow where
  parseNamedRecord r =
    OHLCVRow
      <$> r Csv..: "date"
      <*> r Csv..: "open"
      <*> r Csv..: "high"
      <*> r Csv..: "low"
      <*> r Csv..: "close"
      <*> r Csv..: "volume"

-- ---------------------------------------------------------------------------
-- Conversion helpers

-- | Parse a @YYYY-MM-DD@ date string into a 'UTCTime' (midnight UTC).
parseDate :: String -> Either MarketDataError UTCTime
parseDate s =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
    Nothing  -> Left (ParseError ("Invalid date: " <> s))
    Just day -> Right (UTCTime day (secondsToDiffTime 0))

rowToOHLCV :: Symbol -> OHLCVRow -> Either MarketDataError OHLCV
rowToOHLCV sym row = do
  ts <- parseDate (rowDate row)
  Right OHLCV
    { ohlcvSymbol    = sym
    , ohlcvTimestamp = ts
    , ohlcvOpen      = rowOpen   row
    , ohlcvHigh      = rowHigh   row
    , ohlcvLow       = rowLow    row
    , ohlcvClose     = rowClose  row
    , ohlcvVolume    = rowVolume row
    }

-- ---------------------------------------------------------------------------
-- Public API

-- | Load OHLCV bars from a CSV file with the header:
-- @date,open,high,low,close,volume@
--
-- Returns 'Left' on IO errors, parse failures, or validation failures.
loadCSV :: Symbol -> FilePath -> IO (Either MarketDataError [OHLCV])
loadCSV sym path = do
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case result of
    Left ex  -> return $ Left (NetworkError (show ex))
    Right bs ->
      case Csv.decodeByName bs of
        Left err   -> return $ Left (ParseError err)
        Right (_, rows) -> do
          let converted = mapM (rowToOHLCV sym) (V.toList rows)
          case converted of
            Left e     -> return $ Left e
            Right bars -> return $ validateOHLCV bars
