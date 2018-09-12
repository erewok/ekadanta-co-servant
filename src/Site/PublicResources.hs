module Site.PublicResources where

import           Data.ByteString.Lazy        ( fromStrict )
import           Data.Text                   ( Text )
import           Data.Text.Encoding          ( encodeUtf8 )
import           RIO                  hiding ( Handler )
import           RIO.List                    ( headMaybe )
import           Servant
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
  :<|> "posts" :> Capture "post_id" Text :> Get '[Html] Html
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
  :<|> getPostListPageH
  :<|> getPostH
  :<|> searchResultsPostH
  :<|> aboutGetH
  :<|> contactGetH
  :<|> contactPostH
  :<|> homeH


getPostListH :: EkadantaApp Html
getPostListH = do
  config     <- asks _getConfig
  let query = searchPaginatingQ BlogPost Nothing 0
  postList  <- liftIO $ searchContent config query
  case resourcesResp of
    Left err -> pure . Left $ "Failed looking up content"
    Right result -> do
      


homeH :: EkadantaApp Html
homeH = do
  config     <- asks _getConfig
  resources  <- liftIO $ getResourceHits config searchLasthreeProjectsQ

  case resources of
    Left err -> throwError $ error400 err
    Right projects -> do
      latestPost <- liftIO $ getResourceHits config searchLatestPostQ

      case fmap headMaybe latestPost of
        Left err -> throwError $ error400 err
        Right Nothing -> pure $ homePage projects defaultResource
        Right (Just post) -> pure $ homePage projects post
        

error400 :: Text -> ServantErr
error400 msg = err400 { errBody = fromStrict . encodeUtf8 $ msg }

contactPostH :: ContactForm -> EkadantaApp Html
contactPostH contactF = do
  config        <- asks _getConfig
  liftIO $ sendContact config contactF
  pure $ redirectPage "/thanks"
