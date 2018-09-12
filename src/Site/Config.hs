{-# LANGUAGE TemplateHaskell #-}

module Site.Config where

import           Control.Lens
import           Data.Text
import           GHC.Generics
import           RIO
import           System.Envy
import           System.Log.FastLogger      ( LoggerSet
                                            , flushLogStr
                                            , pushLogStr
                                            , toLogStr
                                            )

type EkadantaApp = RIO EkadantaCtx

data EkadantaCtx = EkadantaCtx {
  _getConfig   :: SiteConfig
  , _getKigger :: LoggerSet
  }

data Environment = Local
                 | Dev
                 | Stage
                 | Test
                 | Prod
                 deriving (Eq, Show)

instance Var Environment where
  toVar = show
  fromVar = \case
    "local" -> Just Local
    "dev"   -> Just Dev
    "stage" -> Just Stage
    "test"  -> Just Test
    "prod"  -> Just Prod
    _       -> Nothing


data SiteConfig = SiteConfig {
  environment     :: !Environment
  , version       :: !Text
  , esHost        :: !Text
  , esPort        :: !Text
  , emailUsername :: !Text
  , emailPasswd   :: !Text
} deriving (Generic, Show)

instance DefConfig SiteConfig where
  defConfig = SiteConfig Prod "test-version" "http://localhost" "9200" "user" "password"

instance FromEnv SiteConfig

makeLenses ''EkadantaCtx