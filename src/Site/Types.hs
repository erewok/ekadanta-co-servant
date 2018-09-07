{-# LANGUAGE TemplateHaskell #-}

module Site.Types where

import           Control.Lens
import           Data.Aeson
import qualified Data.Text                         as T
import           Data.Time.LocalTime
import           GHC.Exts                          (IsList (..))
import           GHC.Generics
import           RIO
import qualified Text.Blaze.Html5                   as H
import           Web.FormUrlEncoded                (Form(..)
                                                  , FromForm(..)
                                                  , ToForm(..))
import           Web.Internal.HttpApiData          (FromHttpApiData(..)
                                                  , toQueryParam)
import           Web.Internal.FormUrlEncoded       (parseUnique)



type PageTotal = Int
type CurrentPage = Int
type PageNum = (PageTotal, CurrentPage)
type AllSiteTags = [Text]

-- | Generic Resource object used across whole site
data Resource = Resource {
  _pubdate :: Text
  , _resourceType :: ResourceType
  , _contentEncoding :: ContentEncoding
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
instance FromForm Resource


-- | Resource Types used on the site: to be stored generically in JSON store (Elasticsearch)

data ResourceType = BlogPost
                   | About
                   | Project
                   deriving (Eq, Show, Generic)

parseResourceType :: T.Text -> Either T.Text ResourceType
parseResourceType val
  | val == "blogpost" = Right BlogPost
  | val == "about"    = Right About
  | val == "project"  = Right Project
  | otherwise         = Left ("Not a ResourceType: " <> val)

instance FromJSON ResourceType
instance ToJSON ResourceType

instance ToForm ResourceType where
  toForm rt = fromList [ (T.pack "_resourceType", T.pack . show $ rt) ]

instance FromHttpApiData ResourceType where
  parseUrlPiece = parseResourceType

instance FromForm ResourceType where
  fromForm frm = 
    let parsed = parseUnique "_resourceType" frm :: Either T.Text T.Text
    in join (parseResourceType <$> parsed)

-- | ContentEncoding for differentiating type of content

data ContentEncoding = ContentMarkdown 
                      | ContentHtml
                      deriving (Eq, Show, Generic, Read)

instance ToForm ContentEncoding where
  toForm ce = fromList [ (T.pack "_contentEncoding", T.pack . show $ ce) ]

parseContentEncoding :: T.Text -> Either T.Text ContentEncoding
parseContentEncoding val
  | val == "markdown" = Right ContentMarkdown
  | val == "html"    = Right ContentHtml
  | otherwise         = Left ("Not a ContentEncoding: " <> val)


instance FromHttpApiData ContentEncoding where
  parseUrlPiece = parseContentEncoding

instance FromForm ContentEncoding where
  fromForm frm = 
    let parsed = parseUnique "_contentEncoding" frm :: Either T.Text T.Text
    in join (parseContentEncoding <$> parsed)
  

instance FromJSON ContentEncoding
instance ToJSON ContentEncoding
    
-- | Lenses

makeLenses ''Resource
