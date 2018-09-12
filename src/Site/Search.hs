module Site.Search where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text                               as T
import qualified Data.UUID                               as UUID
import qualified Data.UUID.V4                            as UUID4
import           Network.HTTP.Client                     hiding (Proxy)
import           RIO
import qualified RIO.HashMap                             as HM
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Site.Config
import           Site.Types


-- | Higher-level useful search functions 
getResourceHits :: SiteConfig -> Value -> IO (Either Text [Resource])
getResourceHits config query = do
  resourcesResp <- searchContent config query
  case resourcesResp of
    Left err -> pure . Left $ "Failed looking up content"
    Right value -> case pullHitsResourceTypes value of
      Nothing -> pure . Left $ "Missing data"
      Just resources -> pure . Right $ resources

pullHitsResourceTypes :: Value -> Maybe [Resource]
pullHitsResourceTypes value = 
  let
    hmaps = value 
      ^? _Object 
      . ix "hits" 
      . ix "hits"
      . _Array 
      ^.. folded 
      . traverse 
      . _Object 
      & map (HM.lookup "_source")
    justVals = hmaps
      ^.. traverse 
      . _Just
  in traverse decoderRing justVals
  where decoderRing val = (decode $ encode val) :: Maybe Resource

pullAggsKey :: Text -> Value -> Maybe (Vector Value)
pullAggsKey key esResult = 
  esResult 
    ^? _Object 
    . ix "aggregations" 
    . _Object 
    . ix key 
    . _Object 
    . ix "buckets" 
    . _Array


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

resourceTypeTerm :: ResourceType -> Value
resourceTypeTerm rt = Object $ HM.fromList [ ( "term", Object $ HM.fromList [( "_resourceType", String . T.pack . show $ rt )] )]

pubDateDescSort :: Value
pubDateDescSort = Object $ HM.fromList [ ( "_pubdate", Object $ HM.fromList [( "order", String "desc")])]

tagsAgg :: Value
tagsAgg = Object $ HM.fromList [ ( "terms", Object $ HM.fromList [( "field", String "_tags"), ("size", Number 1000 )] )]

docTypeCount :: Value
docTypeCount = Object $ HM.fromList [ ( "terms", Object $ HM.fromList [( "field", String "_resourceType" ), ("size", Number 5) ] )]

-- | Client functions for interacting with our index/documents
indexContent :: SiteConfig -> ResourceType -> IO (Either ServantError Value)
indexContent config item = do
  uid <- UUID4.nextRandom
  let indexer = indexSearchContent mkSearchClient
  runSearchClient config $ indexer uid item

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
  :<|> "ekadanta" :> "content" :> Capture "docId" UUID.UUID :> ReqBody '[JSON] ResourceType :> Put '[JSON] Value

data SearchClient = 
  SearchClient 
  {
  createSearchIndex :: Value -> ClientM Value
  , searchIndexContent :: Value -> ClientM Value
  , getSearchContent :: UUID.UUID -> ClientM Value
  , indexSearchContent :: UUID.UUID -> ResourceType -> ClientM Value
  } 

mkSearchClient :: SearchClient
mkSearchClient = 
  let 
    createSearchIndex :<|> searchIndexContent :<|> getSearchContent :<|> indexSearchContent
      = client (Proxy :: Proxy SearchAPI)
  in SearchClient {..}

clientEnv :: SiteConfig -> IO ClientEnv
clientEnv config = do
  baseUrl <- parseBaseUrl $ T.unpack . esHost $ config
  manager <- newManager defaultManagerSettings
  pure $ mkClientEnv manager baseUrl

runSearchClient :: SiteConfig -> ClientM a -> IO (Either ServantError a)
runSearchClient config = (clientEnv config >>=) . runClientM

