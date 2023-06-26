module Site.Search where

import Control.Applicative ( (<|>) )
import Control.Lens
    ( Traversable(traverse), (^..), (^?), (^.), (&), set, at, folded, Ixed(ix) )
import Data.Aeson
    ( fromJSON,
      FromJSON(parseJSON),
      Result(..),
      Value(Number, Object, String),
      ToJSON(toJSON))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap
    (fromList, KeyMap)
import Data.Aeson.Lens
    ( key, _String, AsValue(_Object, _Array) )
import           Data.Aeson.Types         ( parseMaybe, parseEither, Result(..) )
import           Data.Bifunctor           ( first )
import           Data.Maybe               ( fromMaybe, fromJust, isNothing, mapMaybe )
import qualified Data.Text                as T
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID4
import Network.HTTP.Client ( defaultManagerSettings, newManager )
import RIO
    ( ($),
      Eq((==)),
      Monad((>>=)),
      Show(..),
      Applicative(pure),
      Traversable(traverse),
      Generic,
      Semigroup((<>)),
      Int,
      Maybe(..),
      IO,
      Either(..),
      (<$>),
      (.),
      id,
      Proxy(..),
      Text,
      Bool(..),
      mapMaybe,
      Vector )
import qualified RIO.HashMap              as HM
import qualified RIO.List                 as L
import qualified RIO.Vector               as V
import Servant.API
import Servant.Client
    ( ClientError,
      client,
      mkClientEnv,
      runClientM,
      ClientEnv,
      ClientM,
      parseBaseUrl )
import Servant.Server ()
import           Web.FormUrlEncoded                (Form(..)
                                                  , FromForm(..)
                                                  , ToForm(..))

import Site.Config ( SiteConfig(esHost, esPort) )
import Site.Types ( ResourceType(..), Resource(_pid), pid )


-- | Higher-level useful search functions
getDocument :: SiteConfig -> UUID.UUID -> IO (Either Text Resource)
getDocument config uid = do
  resourcesResp <- getContent config uid
  case resourcesResp of
    Left _ -> pure . Left $ "Failed looking up content"
    Right value ->
      case value ^? _Object . ix "_source" of
        Nothing -> pure . Left $ "Failed looking up content"
        Just obj -> pure $ decodeEitherResource obj

getResourceHits :: SiteConfig -> Value -> IO (Either Text [Resource])
getResourceHits config query = do
  resourcesResp <- searchContent config query
  case resourcesResp of
    Left _ -> pure . Left $ "Failed looking up content"
    Right value -> pure . Right $ pullHitsResources value

pullHitsResources :: Value -> [Resource]
pullHitsResources value =
  let
    sources =
      value
        ^? key (fromText "hits")
        . key (fromText "hits")
        . _Array
        ^.. folded
        . traverse
        . key (fromText "_source")
  in mapMaybe decodeMaybeResource sources

decodeEitherResource :: Value -> Either Text Resource
decodeEitherResource val = first T.pack $ parseEither parseJSON val

decodeMaybeResource :: Value -> Maybe Resource
decodeMaybeResource = parseMaybe parseJSON

pullAggsKey :: Text -> Value -> Maybe (Vector Value)
pullAggsKey aggName esResult =
  esResult
    ^? key "aggregations"
    . key (fromText aggName)
    . key "buckets"
    . _Array

getAggBucketKey :: Value -> Maybe Text
getAggBucketKey obj = obj ^? _Object . ix "key" . _String

getKeyCount :: Text -> Maybe (Vector Value) -> Maybe Int
getKeyCount key_ resourceTotals =
  let
    keyTotal = V.filter (\obj -> getAggBucketKey obj == Just key_) <$> resourceTotals
    maybeNum = L.headMaybe $ keyTotal ^.. folded . traverse . ix "doc_count"
  in case fromJSON <$> maybeNum :: Maybe (Result Int) of
    Nothing -> Nothing
    Just (Error _) -> Nothing
    Just (Success intg) -> Just intg


-- | Common queries
searchLasthreeProjectsQ :: Value
searchLasthreeProjectsQ = searchRecentResourcesQ Project 3

searchLatestPostQ :: Value
searchLatestPostQ = searchRecentResourcesQ BlogPost 1

searchAboutQ :: Value
searchAboutQ = searchRecentResourcesQ About 1

searchTagsQ :: [Text] -> Value
searchTagsQ tags = Object $ fromList [
  (fromText "query"
  ,  Object $ fromList [
    (fromText "match",
    Object $ fromList [
      (fromText "_tags", Object $ fromList [
        (fromText "query", String $ T.unwords tags),
        (fromText "operator", String "and" ) ]
      ) ]
    ) ]
  ) ]

searchContentQ :: Text -> Value
searchContentQ query = Object $ fromList [
  ( "query"
  ,  Object $ fromList [
    ("multi_match",
    Object $ fromList [ ("match", String query)
                         , ("fields", toJSON ["_lede" :: Text, "_body" :: Text] ) ]
    )] )
  ]

searchRecentResourcesQ :: ResourceType -> Int -> Value
searchRecentResourcesQ rt n = Object $ fromList [
  ( "from", Number 0 )
  , ( "size", toJSON n )
  , ( "query", resourceTypeTerm rt )
  , ( "sort", pubDateDescSort ) ]


_DEFAULT_PAGE_COUNT :: Int
_DEFAULT_PAGE_COUNT = 25

type PageCount = Maybe Int
type Offset = Int

searchPaginatingQ :: ResourceType -> PageCount -> Offset -> Value
searchPaginatingQ rt Nothing offset = Object $ fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON _DEFAULT_PAGE_COUNT )
  , ( "query", resourceTypeTerm rt )
  , ( "sort", pubDateDescSort )
  , ("aggs", Object $ fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]
searchPaginatingQ rt (Just count) offset = Object $ fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON count )
  , ( "query", resourceTypeTerm rt )
  , ( "sort", pubDateDescSort )
  , ("aggs", Object $ fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]

matchAllQ :: PageCount -> Offset -> Value
matchAllQ Nothing offset = Object $ fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON _DEFAULT_PAGE_COUNT )
  , ( "query", Object $ fromList [("match_all", Object $ fromList [])] )
  , ( "sort", pubDateDescSort )
  , ("aggs", Object $ fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]
matchAllQ (Just count) offset = Object $ fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON count )
  , ( "query", Object $ fromList [("match_all", Object $ fromList [])] )
  , ( "sort", pubDateDescSort )
  , ("aggs", Object $ fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]

resourceTypeTerm :: ResourceType -> Value
resourceTypeTerm rt = Object $ fromList [
  ( "term", Object $ fromList [
    ( "_resourceType", String . T.pack . show $ rt )] )]

pubDateDescSort :: Value
pubDateDescSort = Object $ fromList [
  ( "_pubdate", Object $ fromList [
    ( "order", String "desc")])]

tagsAgg :: Value
tagsAgg = Object $ fromList [
  ( "terms", Object $ fromList [
    ( "field", String "_tags"), ("size", Number 1000 )] )]

docTypeCount :: Value
docTypeCount = Object $ fromList [
  ( "terms", Object $ fromList [
    ( "field", String "_resourceType" ), ("size", Number 5) ] )]

-- | Client functions for interacting with our index/documents
--   Generate a GUID if one is not supplied
indexContent :: SiteConfig -> Maybe UUID.UUID -> Resource -> IO (Either ClientError (Union '[WithStatus 201 Value, WithStatus 200 Value]))
indexContent config Nothing item = do
  -- CREATE A UUID for this Item
  nuid <- UUID4.nextRandom
  let updatedItem = set pid (Just nuid) item
      indexer = indexDocument mkSearchClient
  runSearchClient config $ indexer nuid updatedItem
indexContent config (Just itemUid) item = do
  -- UPDATE this Item at `itemUid`
  let updatedItem = set pid (Just itemUid) item
      indexer = indexDocument mkSearchClient
  runSearchClient config $ indexer itemUid updatedItem


getContent :: SiteConfig -> UUID.UUID -> IO (Either ClientError Value)
getContent config uid =
  runSearchClient config
    . ($ uid)
    . getSearchContent
    $ mkSearchClient

searchContent :: SiteConfig -> Value -> IO (Either ClientError Value)
searchContent config query =
  runSearchClient config
    . ($ query)
    . searchIndexContent
    $ mkSearchClient

createIndexMapping :: SiteConfig -> Maybe Bool -> Value -> IO (Either ClientError Value)
createIndexMapping config includeTypeName mapping =
  -- looks silly but we _always_ want to include this qparam...
  let includeType = Just $ fromMaybe False includeTypeName
      indexCreator = createSearchIndex mkSearchClient
  in runSearchClient config $ indexCreator includeType mapping


-- | Search API and utility client definitions
type SearchAPI =
  "ekadanta" :> QueryParam "include_type_name" Bool :> ReqBody '[JSON] Value :> Put '[JSON] Value
  :<|> "ekadanta" :> "_search" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "ekadanta" :> "_doc" :> Capture "docId" UUID.UUID :> Get '[JSON] Value
  :<|> "ekadanta" :> "_doc" :> Capture "docId" UUID.UUID :> ReqBody '[JSON] Resource :> UVerb 'PUT '[JSON] '[WithStatus 201 Value, WithStatus 200 Value]

data SearchClient =
  SearchClient
  {
  createSearchIndex :: Maybe Bool -> Value -> ClientM Value
  , searchIndexContent :: Value -> ClientM Value
  , getSearchContent :: UUID.UUID -> ClientM Value
  , indexDocument :: UUID.UUID -> Resource -> ClientM (Union '[WithStatus 201 Value, WithStatus 200 Value])
  }

mkSearchClient :: SearchClient
mkSearchClient =
  let
    createSearchIndex :<|> searchIndexContent :<|> getSearchContent :<|> indexDocument
      = client (Proxy :: Proxy SearchAPI)
  in SearchClient {..}

clientEnv :: SiteConfig -> IO ClientEnv
clientEnv config = do
  baseUrl <- parseBaseUrl $ T.unpack $ ( esHost config ) <> ":" <> ( esPort config )
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager baseUrl

runSearchClient :: SiteConfig -> ClientM a -> IO (Either ClientError a)
runSearchClient config = (clientEnv config >>=) . runClientM

-- | forms
data SearchForm  = SearchForm { query :: Text } deriving (Eq, Show, Generic)

instance FromForm SearchForm
instance ToForm SearchForm