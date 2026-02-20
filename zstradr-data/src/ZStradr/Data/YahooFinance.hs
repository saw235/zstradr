module ZStradr.Data.YahooFinance
  ( fetchYahoo
  ) where

import           Control.Exception        (SomeException, try)
import           Data.Aeson               (FromJSON (..), (.:))
import qualified Data.Aeson               as Aeson
import           Data.Time                (UTCTime, nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime,
                                           utcTimeToPOSIXSeconds)
import qualified Data.Text                as T
import           Network.HTTP.Simple      (getResponseBody, httpLBS,
                                           parseRequest, setRequestHeader)

import           ZStradr.Core.Types       (OHLCV (..), Symbol (..))
import           ZStradr.Data.MarketData  (DateRange (..), MarketDataError (..),
                                           validateOHLCV)

-- ---------------------------------------------------------------------------
-- Yahoo Finance JSON response structure

newtype YFResponse = YFResponse YFChart

instance FromJSON YFResponse where
  parseJSON = Aeson.withObject "YFResponse" $ \o ->
    YFResponse <$> o .: "chart"

newtype YFChart = YFChart (Maybe [YFResult])

instance FromJSON YFChart where
  parseJSON = Aeson.withObject "YFChart" $ \o ->
    YFChart <$> o .: "result"

data YFResult = YFResult [Int] YFQuote

instance FromJSON YFResult where
  parseJSON = Aeson.withObject "YFResult" $ \o -> do
    ts         <- o .: "timestamp"
    indicators <- o .: "indicators"
    quotes     <- indicators .: "quote"
    case quotes of
      []    -> fail "No quote data in Yahoo Finance response"
      (q:_) -> return (YFResult ts q)

data YFQuote = YFQuote
  { yfOpen   :: [Maybe Double]
  , yfHigh   :: [Maybe Double]
  , yfLow    :: [Maybe Double]
  , yfClose  :: [Maybe Double]
  , yfVolume :: [Maybe Double]
  }

instance FromJSON YFQuote where
  parseJSON = Aeson.withObject "YFQuote" $ \o ->
    YFQuote
      <$> o .: "open"
      <*> o .: "high"
      <*> o .: "low"
      <*> o .: "close"
      <*> o .: "volume"

-- ---------------------------------------------------------------------------
-- Conversion

-- | Build the Yahoo Finance v8 chart URL for the given symbol and date range.
buildUrl :: Symbol -> DateRange -> String
buildUrl (Symbol sym) dr =
  "https://query1.finance.yahoo.com/v8/finance/chart/"
  <> T.unpack sym
  <> "?period1=" <> show (toUnix (drStart dr))
  <> "&period2=" <> show (toUnix (drEnd dr))
  <> "&interval=1d"
  where
    toUnix :: UTCTime -> Int
    toUnix = round . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

-- | Convert a raw Yahoo Finance result into a list of OHLCV bars.
-- Rows with any null field are silently skipped.
resultToOHLCVs :: Symbol -> YFResult -> [OHLCV]
resultToOHLCVs sym (YFResult tss quote) =
  [ OHLCV
      { ohlcvSymbol    = sym
      , ohlcvTimestamp = posixSecondsToUTCTime (fromIntegral ts)
      , ohlcvOpen      = o
      , ohlcvHigh      = h
      , ohlcvLow       = l
      , ohlcvClose     = c
      , ohlcvVolume    = v
      }
  | (ts, Just o, Just h, Just l, Just c, Just v) <-
      zip6 tss (yfOpen quote) (yfHigh quote) (yfLow quote)
               (yfClose quote) (yfVolume quote)
  ]

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
zip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) =
  (a,b,c,d,e,f) : zip6 as bs cs ds es fs
zip6 _ _ _ _ _ _ = []

-- ---------------------------------------------------------------------------
-- Helpers

-- | 'try' specialised to 'SomeException', avoiding wildcard type annotations.
tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

-- ---------------------------------------------------------------------------
-- Public API

-- | Fetch daily OHLCV data for the given symbol and date range from the
-- Yahoo Finance v8 chart API.
fetchYahoo :: Symbol -> DateRange -> IO (Either MarketDataError [OHLCV])
fetchYahoo sym dr = do
  let url = buildUrl sym dr
  reqResult <- tryAny (parseRequest url)
  case reqResult of
    Left ex   -> return $ Left (NetworkError (show ex))
    Right req -> do
      let req' = setRequestHeader "User-Agent" ["ZStradr/0.1"] req
      httpResult <- tryAny (httpLBS req')
      case httpResult of
        Left ex    -> return $ Left (NetworkError (show ex))
        Right resp -> do
          let body = getResponseBody resp
          case Aeson.eitherDecode body of
            Left err -> return $ Left (ParseError err)
            Right (YFResponse (YFChart mResults)) ->
              case mResults of
                Nothing  -> return $ Left (MissingData "No results in Yahoo Finance response")
                Just []  -> return $ Left (MissingData "Empty results in Yahoo Finance response")
                Just (r:_) -> return $ validateOHLCV (resultToOHLCVs sym r)
