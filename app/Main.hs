module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Either
import           Data.Time.Clock                       ( getCurrentTime )
import           Data.Typeable
import           Network.HTTP.Types
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Handler.Warp           as Warp
import           Prelude                               ( print )
import           RIO
import           Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus))
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           System.Envy
import           System.Log.FastLogger                 ( newStdoutLoggerSet 
                                                       , defaultBufSize
                                                       , pushLogStrLn
                                                       , flushLogStr
                                                       , ToLogStr(..) )

import Site


errorMaker :: SomeException -> Wai.Response
errorMaker someErr = 
  case (cast someErr :: Maybe AppErrors) of
    Just myError -> Wai.responseLBS (toErrStatus myError) [(hContentType, "application/json")] $ encode myError
    Nothing -> Wai.responseLBS internalServerError500 [(hContentType, "application/json")] $ "{\"status\": \"failed\", \"error\": \"" <> (encode $ show someErr) <> "\"}"


main :: IO ()
main = do
  conf <- decodeEnv :: IO (Either String SiteConfig)
  let config = fromRight (defConfig :: SiteConfig) conf
  if environment config == Local then print config else pure ()

  logger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize

  tstamp <- getCurrentTime
  myKey <- generateKey

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
      jwtCfg = defaultJWTSettings myKey
      cookieCfg = if environment config == Local 
                  then defaultCookieSettings{cookieIsSecure=NotSecure
                                           , cookieXsrfSetting = Nothing}
                  -- "If your web application runs no javascript, disabling XSRF entirely may be required."
                  else defaultCookieSettings{cookieXsrfSetting = Nothing}
      cfg = cookieCfg :. jwtCfg :. EmptyContext

  Warp.runSettings settings $ logger $ ekadantaApp cfg cookieCfg jwtCfg ctx