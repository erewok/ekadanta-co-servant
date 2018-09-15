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
  "posts" :> Get '[Html] Html
  :<|> "posts" :> Capture "pgNum" Int :> Get '[Html] Html
  :<|> "posts" :> Capture "post_id" UUID.UUID :> Get '[Html] Html
  :<|> "search" :> ReqBody '[FormUrlEncoded] Text :> Post '[HTML] Html
  :<|> "about" :> Get '[Html] Html
  :<|> "contact" :> Get '[Html] Html
  :<|> "contact" :> ReqBody '[FormUrlEncoded] ContactForm :> Post '[HTML] Html
  :<|> Get '[Html] Html

publicApi :: Proxy PublicApi
publicApi = Proxy

publicHandlers :: ServerT PublicApi EkadantaApp
publicHandlers =
  getPostListH
  :<|> getPostListPageNumH
  :<|> getPostH
  :<|> searchResultsPostH
  :<|> aboutGetH
  :<|> contactGetH
  :<|> contactPostH
  :<|> homeH


-- | Post List result for first page of posts
getPostListH :: EkadantaApp Html
getPostListH = getPostListPageNumH 1


-- | Post list later pages result
getPostListPageNumH :: Int -> EkadantaApp Html
getPostListPageNumH pgNum = do
  let query = searchPaginatingQ BlogPost Nothing $ (pgNum - 1) * _DEFAULT_PAGE_COUNT
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config query
  searchContentListProcessor pgNum resourcesResp BlogPost


-- | Get a particular post by its uid
getPostH :: UUID.UUID -> EkadantaApp Html
getPostH uid = do
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ getDocument config uid
  case resourcesResp of
    Left err -> throwM $ err404 { errBody = "Failed looking up content" }
    Right resource -> pure $ contentDetailPage resource


-- | Post search results from a text input
searchResultsPostH :: Text -> EkadantaApp Html
searchResultsPostH q = do
  let query      = searchContentQ q
  config         <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config query
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
        

error400 :: Text -> ServantErr
error400 msg = err400 { errBody = fromStrict . encodeUtf8 $ msg }
