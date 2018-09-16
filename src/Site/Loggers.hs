module Site.Loggers where


import Data.Aeson
import Data.Aeson.Types                           ( typeMismatch )
import Data.Default
import Data.Time.Clock                            ( UTCTime, getCurrentTime )
import GHC.Generics
import Network.Wai                                ( Middleware )
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import RIO
import System.Log.FastLogger                      ( ToLogStr(..) 
                                                  , LoggerSet
                                                  , flushLogStr
                                                  , pushLogStr )

import Site.Config
import Site.Types

data LogMessage = LogMessage {
  message       :: Text
  , timestamp   :: UTCTime
  , lversion     :: Text
  , level       :: RIO.LogLevel
  , application :: Text
} deriving (Eq, Show, Generic)


instance ToJSON RIO.LogLevel where
  toJSON RIO.LevelDebug = String "debug"
  toJSON RIO.LevelInfo = String "info"
  toJSON RIO.LevelWarn = String "warn"
  toJSON RIO.LevelError = String "error"
  toJSON (RIO.LevelOther msg) = String "msg"

instance FromJSON RIO.LogLevel where
  parseJSON (String "debug") = pure RIO.LevelDebug
  parseJSON (String "info") = pure RIO.LevelInfo
  parseJSON (String "warn") = pure RIO.LevelWarn
  parseJSON (String "error") = pure RIO.LevelError
  parseJSON (String msg) = pure $ RIO.LevelOther msg
  parseJSON invalid = typeMismatch "RIO.LogLevel" invalid

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

jsonRequestLogger :: IO Middleware
jsonRequestLogger = 
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }