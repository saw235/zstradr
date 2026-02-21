{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Load OHLCV bars from a standard CSV file.
--
-- Expected column layout (header row required):
--
-- > date,open,high,low,close,volume
--
-- The @date@ field is interpreted as a Unix millisecond timestamp when it
-- looks like a whole number, or as an ISO-8601 date (@YYYY-MM-DD@) otherwise
-- (converted to the start of that UTC day in milliseconds).
module ZStradr.Data.CSV
  ( CSVError (..)
  , loadCSV
  ) where

import           Control.Exception        (IOException, try)
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Csv                 (FromNamedRecord (..), decodeByName,
                                           (.:))
import qualified Data.Text                as T
import           Data.Text                (Text)
import           Data.Vector              (Vector)
import qualified Data.Vector              as V

import           ZStradr.Core.Types       (OHLCV (..), Symbol, Timestamp (..))

-- ---------------------------------------------------------------------------
-- Error type

-- | Errors that can arise while loading a CSV file.
data CSVError
  = CSVIOError   !IOException
    -- ^ The file could not be read.
  | CSVParseError !String
    -- ^ The CSV could not be parsed (e.g. malformed header or column count).
  | CSVValidationError !String
    -- ^ A row failed business-logic validation.
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Internal row type (cassava record)

data RawRow = RawRow
  { rawDate   :: !Text
  , rawOpen   :: !Double
  , rawHigh   :: !Double
  , rawLow    :: !Double
  , rawClose  :: !Double
  , rawVolume :: !Double
  } deriving (Show)

instance FromNamedRecord RawRow where
  parseNamedRecord r = RawRow
    <$> r .: "date"
    <*> r .: "open"
    <*> r .: "high"
    <*> r .: "low"
    <*> r .: "close"
    <*> r .: "volume"

-- ---------------------------------------------------------------------------
-- Public API

-- | Load OHLCV bars for a given symbol from a CSV file path.
--
-- The file must contain a header row with the columns:
-- @date@, @open@, @high@, @low@, @close@, @volume@.
--
-- Returns @Left CSVError@ if the file cannot be read, parsed, or validated.
loadCSV :: Symbol -> FilePath -> IO (Either CSVError (Vector OHLCV))
loadCSV sym path = do
  result <- try (BL.readFile path) :: IO (Either IOException ByteString)
  case result of
    Left  ioErr -> return (Left (CSVIOError ioErr))
    Right bytes -> return (parseAndValidate sym bytes)

-- ---------------------------------------------------------------------------
-- Internal helpers

parseAndValidate :: Symbol -> ByteString -> Either CSVError (Vector OHLCV)
parseAndValidate sym bytes =
  case decodeByName bytes of
    Left  err  -> Left (CSVParseError err)
    Right (_, rows) ->
      case V.mapM (validateRow sym) rows of
        Left  verr  -> Left (CSVValidationError verr)
        Right ohlcvs -> Right ohlcvs

validateRow :: Symbol -> RawRow -> Either String OHLCV
validateRow sym RawRow{..} = do
  ts <- parseTimestamp rawDate
  if rawOpen  <= 0 then Left ("Non-positive open price: "  <> show rawOpen)  else Right ()
  if rawHigh  <= 0 then Left ("Non-positive high price: "  <> show rawHigh)  else Right ()
  if rawLow   <= 0 then Left ("Non-positive low price: "   <> show rawLow)   else Right ()
  if rawClose <= 0 then Left ("Non-positive close price: " <> show rawClose) else Right ()
  if rawVolume <= 0 then Left ("Non-positive volume: "     <> show rawVolume) else Right ()
  if rawHigh < rawLow
    then Left ("High < Low: " <> show rawHigh <> " < " <> show rawLow)
    else Right ()
  return OHLCV
    { ohlcvSymbol    = sym
    , ohlcvTimestamp = ts
    , ohlcvOpen      = rawOpen
    , ohlcvHigh      = rawHigh
    , ohlcvLow       = rawLow
    , ohlcvClose     = rawClose
    , ohlcvVolume    = rawVolume
    }

-- | Parse a date string as either a Unix millisecond integer or an ISO-8601
-- @YYYY-MM-DD@ date (midnight UTC in milliseconds).
parseTimestamp :: Text -> Either String Timestamp
parseTimestamp txt
  | T.all (\c -> c == '-' || (c >= '0' && c <= '9')) txt
  , Just ms <- readMaybe (T.unpack txt) =
      Right (Timestamp ms)
  | [yyyy, mm, dd] <- T.splitOn "-" txt
  , Just y  <- readMaybe (T.unpack yyyy)
  , Just mo <- readMaybe (T.unpack mm)
  , Just d  <- readMaybe (T.unpack dd) =
      Right (Timestamp (isoToMillis y mo d))
  | otherwise =
      Left ("Cannot parse date: " <> T.unpack txt)

-- | Convert a @YYYY-MM-DD@ date to Unix milliseconds (midnight UTC).
isoToMillis :: Int -> Int -> Int -> Integer
isoToMillis y m d =
  fromIntegral (daysFromEpoch y m d) * 86_400_000

-- | Days elapsed since the Unix epoch (1970-01-01) for the given Gregorian
-- calendar date.  Uses the standard all-integer Julian Day Number formula.
--
-- Julian Day of Unix epoch: 2440588
--
-- Algorithm: Meeus, "Astronomical Algorithms" §7.
daysFromEpoch :: Int -> Int -> Int -> Int
daysFromEpoch year month day =
  let a   = (14 - month) `div` 12
      y   = year + 4800 - a
      m   = month + 12 * a - 3
      jdn = day
            + (153 * m + 2) `div` 5
            + 365 * y
            + y `div` 4
            - y `div` 100
            + y `div` 400
            - 32045
  in  jdn - 2440588

-- | A local 'readMaybe' to avoid the extra @read@ dependency.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
