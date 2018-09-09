module Site.Config where

import Data.Text
import GHC.Generics
import RIO
import System.Envy


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
  environment :: Environment
  , version   :: Text
  , esHost    :: Text
  , esPort    :: Text
} deriving (Generic, Show)

instance DefConfig SiteConfig where
  defConfig = SiteConfig Prod "test-version" "http://localhost" "9200"

instance FromEnv SiteConfig
