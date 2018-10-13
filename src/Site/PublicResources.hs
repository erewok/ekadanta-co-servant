module Site.PublicResources where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.UUID                as UUID
import           RIO                  hiding ( Handler )
import           RIO.List                    ( headMaybe )
import qualified RIO.HashMap              as HM
import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.HTML.Blaze
import qualified Servant.Checked.Exceptions as SCE
import           Text.Blaze.Html             ( Html )

import           Site.Config
import           Site.Exceptions
import           Site.Search
import           Site.Types
import           Site.Html.Contact
import           Site.Html.Home
import           Site.Html.ContentDetail
import           Site.Html.ContentList


type PublicApi = 
  "posts" :> SCE.Throws AppErrors :> Get '[HTML] Html
  :<|> "projects" :> SCE.Throws AppErrors :> Get '[HTML] Html
  :<|> "posts" :> Capture "pgNum" Int :> SCE.Throws AppErrors :> Get '[HTML] Html
  :<|> "projects" :> Capture "pgNum" Int :> SCE.Throws AppErrors :> Get '[HTML] Html
  :<|> "posts" :> Capture "post_id" UUID.UUID :> SCE.Throws AppErrors :> Get '[HTML] Html
  :<|> "projects" :> Capture "post_id" UUID.UUID :> SCE.Throws AppErrors :> Get '[HTML] Html
  :<|> "search" :> ReqBody '[FormUrlEncoded] SearchForm :> Post '[HTML] Html
  :<|> "about" :> Get '[HTML] Html
  :<|> "contact" :>  Get '[HTML] Html
  :<|> "contact" :> ReqBody '[FormUrlEncoded] ContactForm :> Post '[HTML] Html
  :<|> Get '[HTML] Html
  

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


-- | Generic retrieval handler that will lookup content and return Detail Page for it
getPaginatedContent :: ResourceType -> Int -> EkadantaApp (SCE.Envelope '[AppErrors] Html)
getPaginatedContent rt pgNum = do
  let query = searchPaginatingQ rt Nothing $ (pgNum - 1) * _DEFAULT_PAGE_COUNT
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config query
  case searchContentListProcessor pgNum resourcesResp rt of
    Left err ->  SCE.pureErrEnvelope ContentLoadFailure
    Right render -> SCE.pureSuccEnvelope render

searchContentListProcessor :: Int -> Either ServantError Value -> ResourceType -> Either T.Text Html
searchContentListProcessor pgNum searchResult rt =
  case searchResult of
    Left err -> Left "Failed looking up content"
    Right result -> do
      let resources = pullHitsResources result
          resourceTotals = pullAggsKey "counts" result
          postTotal =  getKeyCount (T.pack . show $ rt) resourceTotals
          pageCount = fromMaybe 0 postTotal
          tagCounts = pullAggsKey "tags" result
          tagList = tagCounts ^.. folded . traverse . (_Object . ix "key" . _String)
      pure $ contentListPage (pageCount, pgNum) rt tagList resources

-- | Get a particular post by its uid
getResourceH :: UUID.UUID -> EkadantaApp (SCE.Envelope '[AppErrors] Html)
getResourceH uid = do
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ getDocument config uid
  case resourcesResp of
    Left err -> SCE.pureErrEnvelope MissingContent
    Right resource -> SCE.pureSuccEnvelope $ contentDetailPage resource


-- | Post search results from a text input
searchResultsPostH :: SearchForm -> EkadantaApp Html
searchResultsPostH sform = do
  let q          = searchContentQ ( query sform )
  config         <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config q
  case searchContentListProcessor 0 resourcesResp BlogPost of
    Left err -> throwM ContentLoadFailure
    Right rendered -> pure rendered


-- | About page, also from search results
aboutGetH :: EkadantaApp Html
aboutGetH = do
  config         <- asks _getConfig
  latestAboutPage  <- liftIO $ getResourceHits config searchAboutQ
  case fmap headMaybe latestAboutPage of
    Left err -> throwM ContentLoadFailure
    Right Nothing -> throwM MissingContent
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
    Left err -> throwM ContentLoadFailure
    Right projects -> do
      latestPost <- liftIO $ getResourceHits config searchLatestPostQ

      case fmap headMaybe latestPost of
        Left err -> throwM ContentLoadFailure
        Right Nothing -> pure $ homePage projects defaultResource
        Right (Just post) -> pure $ homePage projects post

