module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Either
import           Data.Time.Clock                     ( getCurrentTime )
import           Network.HTTP.Types
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Handler.Warp           as Warp
import           Prelude                              ( print )
import           RIO
import           System.Envy
import           System.Log.FastLogger                ( newStdoutLoggerSet 
                                                      , defaultBufSize
                                                      , pushLogStrLn
                                                      , flushLogStr
                                                      , ToLogStr(..) )

import Site


errorMaker :: SomeException -> Wai.Response
errorMaker someErr = 
  let showErr = LBS.pack . show $ someErr
      errMsg = "{\"status\": \"failure\", \"error\": \"" <> showErr <> "\"}"
  in Wai.responseLBS internalServerError500 [(hContentType, "application/json")] errMsg


main :: IO ()
main = do
  conf <- decodeEnv :: IO (Either String SiteConfig)
  let config = fromRight (defConfig :: SiteConfig) conf
  if environment config == Local then print config else pure ()

  logger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize

  tstamp <- getCurrentTime

  let lgmsg = LogMessage {
    message = "Ekadanta App Starting up"
    , timestamp = tstamp
    , lversion = version config
    , application = "ekadanta"
    , level = RIO.LevelInfo
  }
  pushLogStrLn appLogger (toLogStr lgmsg) >> flushLogStr appLogger

  let ctx = EkadantaCtx config appLogger

      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort 8000 warpSettings
      timeoutSettings = Warp.setTimeout 55 portSettings
      settings = Warp.setOnExceptionResponse errorMaker timeoutSettings

  Warp.runSettings settings $ logger $ ekadantaApp ctx