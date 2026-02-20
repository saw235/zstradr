{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Yahoo Finance market data adapter.
--
-- Fetches historical daily OHLCV bars from the Yahoo Finance v8 chart API:
--
-- > GET https://query1.finance.yahoo.com/v8/finance/chart/{symbol}
-- >   ?period1={unix_start}&period2={unix_end}&interval=1d
--
-- No API key is required for this endpoint.
module ZStradr.Data.Yahoo
  ( DateRange (..)
  , YahooError (..)
  , fetchOHLCV
  ) where

import           Control.Exception         (IOException, try)
import qualified Data.ByteString.Lazy      as BL
import           Data.Aeson                (FromJSON, ToJSON, Value (..))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as AT
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (newTlsManager)

import           ZStradr.Core.Types        (OHLCV (..), Symbol (..), Timestamp (..))

-- ---------------------------------------------------------------------------
-- Public types

-- | A date range expressed as Unix timestamps in seconds.
data DateRange = DateRange
  { drFrom :: !Integer  -- ^ Inclusive start (Unix seconds).
  , drTo   :: !Integer  -- ^ Exclusive end   (Unix seconds).
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | Errors that can arise while fetching from Yahoo Finance.
data YahooError
  = YahooNetworkError !String
    -- ^ HTTP or TCP failure.
  | YahooParseError   !String
    -- ^ The JSON response could not be decoded.
  | YahooNoData
    -- ^ The response contained no bars for the requested range.
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Public API

-- | Fetch daily OHLCV bars for a symbol over the given date range.
--
-- Returns @Left YahooError@ on network or parse failure.
fetchOHLCV
  :: Symbol
  -> DateRange
  -> IO (Either YahooError [OHLCV])
fetchOHLCV sym@(Symbol ticker) DateRange{..} = do
  mgr <- newTlsManager
  let url = T.unpack $
        "https://query1.finance.yahoo.com/v8/finance/chart/"
        <> ticker
        <> "?period1=" <> T.pack (show drFrom)
        <> "&period2=" <> T.pack (show drTo)
        <> "&interval=1d"
  reqResult <- try (parseRequest url) :: IO (Either IOException Request)
  case reqResult of
    Left  err -> return (Left (YahooNetworkError (show err)))
    Right req -> do
      let req' = req
            { requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; ZStradr/0.1)")
                , ("Accept",     "application/json")
                ]
            }
      httpResult <- try (httpLbs req' mgr)
                      :: IO (Either IOException (Response BL.ByteString))
      case httpResult of
        Left  err  -> return (Left (YahooNetworkError (show err)))
        Right resp -> return (parseYahooResponse sym (responseBody resp))

-- ---------------------------------------------------------------------------
-- JSON parsing

parseYahooResponse :: Symbol -> BL.ByteString -> Either YahooError [OHLCV]
parseYahooResponse sym bytes =
  case Aeson.eitherDecode bytes of
    Left  err -> Left (YahooParseError ("JSON decode error: " <> err))
    Right val ->
      case AT.parseEither (parseChart sym) val of
        Left  err    -> Left (YahooParseError err)
        Right ohlcvs -> Right ohlcvs

-- | Navigate the Yahoo Finance v8 JSON structure:
--
-- > { "chart": { "result": [ { "timestamp": [...],
-- >                             "indicators": {
-- >                               "quote": [ { "open":   [...]
-- >                                          , "high":   [...]
-- >                                          , "low":    [...]
-- >                                          , "close":  [...]
-- >                                          , "volume": [...] } ] } } ] } }
parseChart :: Symbol -> Value -> AT.Parser [OHLCV]
parseChart sym = Aeson.withObject "root" $ \root -> do
  chart   <- root  AT..: "chart"
  results <- chart AT..: "result"
  case results of
    []    -> pure []
    (r:_) -> do
      timestamps <- r AT..: "timestamp" :: AT.Parser [Integer]
      indicators <- r AT..: "indicators"
      quotes     <- indicators AT..: "quote"
      case quotes of
        []    -> pure []
        (q:_) -> do
          opens   <- q AT..: "open"   :: AT.Parser [Maybe Double]
          highs   <- q AT..: "high"   :: AT.Parser [Maybe Double]
          lows    <- q AT..: "low"    :: AT.Parser [Maybe Double]
          closes  <- q AT..: "close"  :: AT.Parser [Maybe Double]
          volumes <- q AT..: "volume" :: AT.Parser [Maybe Double]
          let rows = zip6 timestamps opens highs lows closes volumes
          return
            [ OHLCV
                { ohlcvSymbol    = sym
                , ohlcvTimestamp = Timestamp (ts * 1000)  -- seconds → milliseconds
                , ohlcvOpen      = o
                , ohlcvHigh      = h
                , ohlcvLow       = l
                , ohlcvClose     = c
                , ohlcvVolume    = v
                }
            | (ts, Just o, Just h, Just l, Just c, Just v) <- rows
            ]

-- ---------------------------------------------------------------------------
-- Utilities

-- | 'zip' for six lists.
zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) =
  (a, b, c, d, e, f) : zip6 as bs cs ds es fs
zip6 _ _ _ _ _ _ = []
