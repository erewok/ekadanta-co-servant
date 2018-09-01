{-# LANGUAGE TemplateHaskell #-}

module Site.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Time.LocalTime
import           GHC.Generics
import           RIO
import qualified Text.Blaze.Html5            as H


type PageTotal = Int
type CurrentPage = Int
type PageNum = (PageTotal, CurrentPage)
type AllSiteTags = [Text]


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
  , _published :: Bool
  , _body :: Text
  , _title :: Text
  , _lede  :: Text
  , _tags :: [Text]
  , _pid :: Text
} deriving (Eq, Show, Generic)

instance FromJSON Resource
instance ToJSON Resource

makeLenses ''Resource
