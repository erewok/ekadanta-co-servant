module Main where

import System.Directory ( doesFileExist )
import System.Environment ( getArgs )

import Data.Aeson (eitherDecodeFileStrict', Value)
import qualified Data.Text as T
import RIO
    ( FilePath, Either(..), IO, Maybe(..), String,
      (<$>), (<>), (>>), (>>=), (||), ($), (.),
      map,
      mapM, mapM_,
      first,
      id,
      not,
      fromRight,
      and,
      concat,
      null,
      pure,
      undefined,
      exitWith,
      show,
      ExitCode(ExitFailure),
      LogLevel(..) )
-- import RIO.Process
import RIO.Time (getCurrentTime)
import Servant.Client ( ClientError )
import System.Envy (decodeEnv, DefConfig(..))
import System.Log.FastLogger
  ( newStdoutLoggerSet,
    defaultBufSize,
    pushLogStrLn,
    flushLogStr,
    LoggerSet,
    ToLogStr(..) )

import Site
    ( SiteConfig(environment, version),
      Environment(Local),
      LogMessage(..),
      jsonRequestLogger )
import Site.Config
import Site.Search ( indexContent )
import Site.Types ( Resource )


logSender :: ToLogStr msg => LoggerSet -> msg -> IO ()
logSender appLogger lgMsg = pushLogStrLn appLogger (toLogStr lgMsg) >> flushLogStr appLogger

jsonFileToResource :: FilePath -> IO (Either String Resource)
jsonFileToResource = eitherDecodeFileStrict'

fpathToLogMsg :: LogMessage -> String -> LogMessage
fpathToLogMsg lgMsg fp = lgMsg {message = "Indexing file: " <> T.pack fp}

mkFailMsg :: LogMessage -> IO LogMessage
mkFailMsg ogLgMsg = do
  tstamp <- getCurrentTime
  pure LogMessage {
    message = "FAIL:  "
    , timestamp = tstamp
    , lversion = lversion ogLgMsg
    , application = "ekadanta indexer"
    , level = RIO.LevelError
  }

indexer :: LogMessage -> SiteConfig -> (LogMessage -> IO a) -> FilePath -> IO (Either T.Text Value)
indexer ogLgMsg config logger fp = do
  _ <- logger $ ogLgMsg { message = "Indexing document: " <> T.pack fp }
  maybeRes <- jsonFileToResource fp
  failMsg <- mkFailMsg ogLgMsg
  case maybeRes of
    (Right res) -> do
      indexRes <- indexContent config Nothing res
      case indexRes of
        Left fail' -> do
          _ <- logger $ failMsg { message = message failMsg <> T.pack fp }
          pure $ (Left . T.pack . show ) fail'
        Right success -> pure $ Right success
    Left err -> do
      _ <- logger $ failMsg { message = "Failed to decode file " <> T.pack fp <> " " <> T.pack err}
      pure (Left ("Failed to decode file " <> T.pack err))

main :: IO ()
main = do
  itemPaths <- getArgs
  let itemPathsT = map T.pack itemPaths
      itemPathsStr = T.intercalate ", " itemPathsT
  conf <- decodeEnv :: IO (Either String SiteConfig)
  let config = fromRight (defConfig :: SiteConfig) conf
  tstamp <- getCurrentTime
  let lgMsg = LogMessage {
    message = "Ekadanta App Indexer Script Running with New Doc Path: " <> itemPathsStr
    , timestamp = tstamp
    , lversion = version config
    , application = "ekadanta indexer"
    , level = RIO.LevelInfo
  }
  appLogger <- newStdoutLoggerSet defaultBufSize
  -- Get args with item paths and check if they're all present
  allPresent <- and <$> mapM doesFileExist itemPaths
  if not allPresent || null itemPaths
    then do
      let lgMsg' = lgMsg { message = "One or more files has failed to exist " <> itemPathsStr }
      pushLogStrLn appLogger (toLogStr lgMsg') >> flushLogStr appLogger
      exitWith (ExitFailure 1)
    else do
      let logger = logSender appLogger
      mapM_ (indexer lgMsg config logger) itemPaths
