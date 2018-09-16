module Site.PublicResources where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy        ( fromStrict )
import           Data.Maybe
import           Data.Text                   ( Text, pack )
import           Data.Text.Encoding          ( encodeUtf8 )
import qualified Data.UUID                as UUID
import           RIO                  hiding ( Handler )
import           RIO.List                    ( headMaybe )
import qualified RIO.HashMap              as HM
import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.HTML.Blaze
import           Text.Blaze.Html             ( Html )

import           Site.Config
import           Site.Search
import           Site.Types
import           Site.Html.Contact
import           Site.Html.Home
import           Site.Html.ContentDetail
import           Site.Html.ContentList


type PublicApi = 
  "posts" :> Get '[HTML] Html
  :<|> "projects" :> Get '[HTML] Html
  :<|> "posts" :> Capture "pgNum" Int :> Get '[HTML] Html
  :<|> "projects" :> Capture "pgNum" Int :> Get '[HTML] Html
  :<|> "posts" :> Capture "post_id" UUID.UUID :> Get '[HTML] Html
  :<|> "projects" :> Capture "post_id" UUID.UUID :> Get '[HTML] Html
  :<|> "search" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] Html
  :<|> "about" :> Get '[HTML] Html
  :<|> "contact" :> Get '[HTML] Html
  :<|> "contact" :> ReqBody '[FormUrlEncoded] ContactForm :> Post '[HTML] Html
  :<|> Get '[HTML] Html
  :<|> "static" :> Raw

publicApi :: Proxy PublicApi
publicApi = Proxy

publicHandlers :: ServerT PublicApi EkadantaApp
publicHandlers =
  getPaginatedContent BlogPost 1
  :<|> getPaginatedContent Project 1
  :<|> getPaginatedContent BlogPost
  :<|> getPaginatedContent Project
  :<|> getResourceH
  :<|> getResourceH
  :<|> searchResultsPostH
  :<|> aboutGetH
  :<|> contactGetH
  :<|> contactPostH
  :<|> homeH
  :<|> serveDirectoryFileServer "static"

-- | Generic retrieval handler that will lookup content and return Detail Page for it
getPaginatedContent :: ResourceType -> Int -> EkadantaApp Html
getPaginatedContent rt pgNum = do
  let query = searchPaginatingQ rt Nothing $ (pgNum - 1) * _DEFAULT_PAGE_COUNT
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config query
  searchContentListProcessor pgNum resourcesResp rt

searchContentListProcessor :: Int -> Either ServantError Value -> ResourceType -> EkadantaApp Html
searchContentListProcessor pgNum searchResult rt =
  case searchResult of
    Left err -> throwM $ err400 { errBody = "Failed looking up content" }
    Right result -> do
      let resources = pullHitsResources result
          resourceTotals = pullAggsKey "counts" result
          postTotal =  getKeyCount (pack . show $ rt) resourceTotals
          pageCount = if isJust postTotal then fromJust postTotal else 0
          tagCounts = pullAggsKey "tags" result
          tagList = tagCounts ^.. folded . traverse . (_Object . ix "key" . _String)
      pure $ contentListPage (pageCount, pgNum) rt tagList resources

-- | Get a particular post by its uid
getResourceH :: UUID.UUID -> EkadantaApp Html
getResourceH uid = do
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ getDocument config uid
  case resourcesResp of
    Left err -> throwM $ err404 { errBody = "Failed looking up content" }
    Right resource -> pure $ contentDetailPage resource


-- | Post search results from a text input
searchResultsPostH :: SearchForm -> EkadantaApp Html
searchResultsPostH sform = do
  let q          = searchContentQ ( query sform )
  config         <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config q
  searchContentListProcessor 0 resourcesResp BlogPost


-- | About page, also from search results
aboutGetH :: EkadantaApp Html
aboutGetH = do
  config         <- asks _getConfig
  latestAboutPage  <- liftIO $ getResourceHits config searchAboutQ
  case fmap headMaybe latestAboutPage of
    Left err -> throwM $ error400 err
    Right Nothing -> throwM $ err404 { errBody = "Failed looking up content" }
    Right (Just post) -> pure $ contentDetailPage post


-- | Contact form page
contactGetH :: EkadantaApp Html
contactGetH = pure $ contactPage True


-- | Contact form post
contactPostH :: ContactForm -> EkadantaApp Html
contactPostH contactF = do
  config        <- asks _getConfig
  liftIO $ sendContact config contactF
  pure $ redirectPage "/thanks"

-- | Home page
homeH :: EkadantaApp Html
homeH = do
  config     <- asks _getConfig
  resources  <- liftIO $ getResourceHits config searchLasthreeProjectsQ

  case resources of
    Left err -> throwM $ error400 err
    Right projects -> do
      latestPost <- liftIO $ getResourceHits config searchLatestPostQ

      case fmap headMaybe latestPost of
        Left err -> throwM $ error400 err
        Right Nothing -> pure $ homePage projects defaultResource
        Right (Just post) -> pure $ homePage projects post


-- | Helper functions, not handlers
    

error400 :: Text -> ServantErr
error400 msg = err400 { errBody = fromStrict . encodeUtf8 $ msg }
