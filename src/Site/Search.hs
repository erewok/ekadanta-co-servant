module Site.Search where

import           Data.Aeson
import qualified Data.Text                               as T
import qualified Data.UUID                               as UUID
import qualified Data.UUID.V4                            as UUID4
import           Network.HTTP.Client                     hiding (Proxy)
import           RIO
import qualified RIO.HashMap                             as HM
import           Servant.API
import           Servant.Client

import           Site.Config
import           Site.Types

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

resourceTypeTerm :: ResourceType -> Value
resourceTypeTerm rt = Object $ HM.fromList [ ( "term", Object $ HM.fromList [( "_resourceType", String . T.pack . show $ rt )] )]

pubDateDescSort :: Value
pubDateDescSort = Object $ HM.fromList [ ( "_pubdate", Object $ HM.fromList [( "order", String "desc")])]


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

