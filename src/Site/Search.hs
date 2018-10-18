module Site.Search where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types         ( parseMaybe, parseEither, Result(..) )
import           Data.Bifunctor           ( first )
import           Data.Maybe               ( catMaybes )
import qualified Data.Text                as T
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID4
import           Network.HTTP.Client      hiding (Proxy)
import           RIO                      hiding ( first )
import qualified RIO.HashMap              as HM
import qualified RIO.List                 as L
import qualified RIO.Vector               as V
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Web.FormUrlEncoded                (Form(..)
                                                  , FromForm(..)
                                                  , ToForm(..))

import           Site.Config
import           Site.Types


-- | Higher-level useful search functions
getDocument :: SiteConfig -> UUID.UUID -> IO (Either Text Resource)
getDocument config uid = do
  resourcesResp <- getContent config uid
  case resourcesResp of
    Left err -> pure . Left $ "Failed looking up content"
    Right value ->
      case value ^? _Object . ix "_source" of
        Nothing -> pure . Left $ "Failed looking up content"
        Just obj -> pure $ decodeEitherResource obj

getResourceHits :: SiteConfig -> Value -> IO (Either Text [Resource])
getResourceHits config query = do
  resourcesResp <- searchContent config query
  case resourcesResp of
    Left err -> pure . Left $ "Failed looking up content"
    Right value -> pure . Right $ pullHitsResources value

pullHitsResources :: Value -> [Resource]
pullHitsResources value = 
  let
    sources = 
      value 
        ^? key "hits" 
        . key "hits"
        . _Array 
        ^.. folded 
        . traverse 
        . (key "_source")
  in catMaybes $ map decodeMaybeResource sources

decodeEitherResource :: Value -> Either Text Resource
decodeEitherResource val = first T.pack $ parseEither parseJSON val

decodeMaybeResource :: Value -> Maybe Resource
decodeMaybeResource = parseMaybe parseJSON 

pullAggsKey :: Text -> Value -> Maybe (Vector Value)
pullAggsKey aggName esResult = 
  esResult 
    ^? key "aggregations"
    . key aggName
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
searchTagsQ tags = Object $ HM.fromList [
  ( "query"
  ,  Object $ HM.fromList [ 
    ("match",
    Object $ HM.fromList [ 
      ("_tags", Object $ HM.fromList [ 
        ("query", String $ T.unwords tags),
        ("operator", String "and" ) ]
      ) ]
    ) ]
  ) ]

searchContentQ :: Text -> Value
searchContentQ query = Object $ HM.fromList [
  ( "query"
  ,  Object $ HM.fromList [ 
    ("multi_match",
    Object $ HM.fromList [ ("match", String query)
                         , ("fields", toJSON ["_lede" :: Text, "_body" :: Text] ) ]
    )] )
  ]

searchRecentResourcesQ :: ResourceType -> Int -> Value
searchRecentResourcesQ rt n = Object $ HM.fromList [
  ( "from", Number 0 )
  , ( "size", toJSON n )
  , ( "query", resourceTypeTerm rt )
  , ( "sort", pubDateDescSort ) ]


_DEFAULT_PAGE_COUNT :: Int
_DEFAULT_PAGE_COUNT = 25

type PageCount = Maybe Int
type Offset = Int

searchPaginatingQ :: ResourceType -> PageCount -> Offset -> Value
searchPaginatingQ rt Nothing offset = Object $ HM.fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON _DEFAULT_PAGE_COUNT )
  , ( "query", resourceTypeTerm rt )
  , ( "sort", pubDateDescSort ) 
  , ("aggs", Object $ HM.fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]
searchPaginatingQ rt (Just count) offset = Object $ HM.fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON count )
  , ( "query", resourceTypeTerm rt )
  , ( "sort", pubDateDescSort ) 
  , ("aggs", Object $ HM.fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]

matchAllQ :: PageCount -> Offset -> Value
matchAllQ Nothing offset = Object $ HM.fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON _DEFAULT_PAGE_COUNT )
  , ( "query", Object $ HM.fromList [("match_all", Object $ HM.fromList [])] )
  , ( "sort", pubDateDescSort ) 
  , ("aggs", Object $ HM.fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]
matchAllQ (Just count) offset = Object $ HM.fromList [
  ( "from", toJSON offset )
  , ( "size", toJSON count )
  , ( "query", Object $ HM.fromList [("match_all", Object $ HM.fromList [])] )
  , ( "sort", pubDateDescSort ) 
  , ("aggs", Object $ HM.fromList [ ("tags", tagsAgg), ("counts", docTypeCount) ] )
  ]

resourceTypeTerm :: ResourceType -> Value
resourceTypeTerm rt = Object $ HM.fromList [ ( "term", Object $ HM.fromList [( "_resourceType", String . T.pack . show $ rt )] )]

pubDateDescSort :: Value
pubDateDescSort = Object $ HM.fromList [ ( "_pubdate", Object $ HM.fromList [( "order", String "desc")])]

tagsAgg :: Value
tagsAgg = Object $ HM.fromList [ ( "terms", Object $ HM.fromList [( "field", String "_tags"), ("size", Number 1000 )] )]

docTypeCount :: Value
docTypeCount = Object $ HM.fromList [ ( "terms", Object $ HM.fromList [( "field", String "_resourceType" ), ("size", Number 5) ] )]

-- | Client functions for interacting with our index/documents
indexContent :: SiteConfig -> Maybe UUID.UUID -> Resource -> IO (Either ServantError Value)
indexContent config muid item = do
  let indexer = indexDocument mkSearchClient
  case muid of
    Just truid -> runSearchClient config $ indexer truid item
    Nothing -> do
      nuid <- UUID4.nextRandom
      let updateItem = item{_pid = UUID.toText nuid}
      runSearchClient config $ indexer nuid updateItem

getContent :: SiteConfig -> UUID.UUID -> IO (Either ServantError Value)
getContent config uid = 
  runSearchClient config 
    . ($ uid)
    . getSearchContent
    $ mkSearchClient

searchContent :: SiteConfig -> Value -> IO (Either ServantError Value)
searchContent config query = 
  runSearchClient config 
    . ($ query)
    . searchIndexContent
    $ mkSearchClient

createIndexMapping :: SiteConfig -> Value -> IO (Either ServantError Value)
createIndexMapping config mapping = 
  runSearchClient config 
    . ($ mapping)
    . createSearchIndex
    $ mkSearchClient

-- | Search API and utility client definitions
type SearchAPI = 
  "ekadanta" :> ReqBody '[JSON] Value :> Put '[JSON] Value
  :<|> "ekadanta" :> "content" :> "_search" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "ekadanta" :> "content" :> Capture "docId" UUID.UUID :> Get '[JSON] Value
  :<|> "ekadanta" :> "content" :> Capture "docId" UUID.UUID :> ReqBody '[JSON] Resource :> Put '[JSON] Value

data SearchClient = 
  SearchClient 
  {
  createSearchIndex :: Value -> ClientM Value
  , searchIndexContent :: Value -> ClientM Value
  , getSearchContent :: UUID.UUID -> ClientM Value
  , indexDocument :: UUID.UUID -> Resource -> ClientM Value
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

runSearchClient :: SiteConfig -> ClientM a -> IO (Either ServantError a)
runSearchClient config = (clientEnv config >>=) . runClientM

-- | forms
data SearchForm  = SearchForm { query :: Text } deriving (Eq, Show, Generic)

instance FromForm SearchForm
instance ToForm SearchForm