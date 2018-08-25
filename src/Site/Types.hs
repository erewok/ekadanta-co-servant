{-# LANGUAGE TemplateHaskell #-}

module Site.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Time.LocalTime
import           GHC.Generics
import           RIO
import qualified Text.Blaze.Html5            as H


data ResourceType = BlogPost
                   | About
                   | Project
                   deriving (Eq, Show, Generic)

instance FromJSON ResourceType
instance ToJSON ResourceType

data Resource = Resource {
    _pubdate :: Text
    , _resourceType :: ResourceType
    , _featuredImage :: Maybe Text
    , _body :: Text
    , _title :: Text
    , _lede  :: Maybe Text
    , _tags :: [Text]
    , _published :: Bool
    , _pid :: Text
} deriving (Eq, Show, Generic)

instance FromJSON Resource
instance ToJSON Resource

makeLenses ''Resource
