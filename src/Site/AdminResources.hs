module Site.AdminResources where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.UUID                as UUID
import           GHC.Generics
import           RIO                  hiding ( Handler, (^..) )
import           Servant
import           Servant.Client
import           Servant.Auth.Server
import           Servant.HTML.Blaze
import           Text.Blaze.Html             ( Html )

import           Site.Config
import           Site.Exceptions
import           Site.Search
import           Site.Types
import           Site.Html.Admin

type AdminAndLogin auths = (Auth auths AdminUser :> AdminApi) :<|> LoginApi

adminServer :: CookieSettings -> JWTSettings -> ServerT (AdminAndLogin auths) EkadantaApp
adminServer cs jwts = adminHandlers :<|> loginHandlers cs jwts

type AdminApi =
  "admin" :> Get '[HTML] Html
    :<|> "admin" :> Capture "pgNum" Int :> Get '[HTML] Html
    :<|> "admin" :> "item" :> Get '[HTML] Html
    :<|> "admin" :> "item" :> ReqBody '[FormUrlEncoded] Resource :> Post '[HTML] Html
    :<|> "admin" :> "item" :> Capture "item_id" UUID.UUID :> Get '[HTML] Html
    :<|> "admin" :> "item" :> Capture "item_id" UUID.UUID :> ReqBody '[FormUrlEncoded] Resource :> Post '[HTML] Html

adminHandlers :: AuthResult AdminUser -> ServerT AdminApi EkadantaApp
adminHandlers (Servant.Auth.Server.Authenticated user) =
  adminListItemsPaginatedH 1
  :<|> adminListItemsPaginatedH
  :<|> adminCreateItemGetH
  :<|> adminCreateItemPostH
  :<|> adminUpdateItemGetH
  :<|> adminUpdateItemPostH
adminHandlers _ =
  noAuthH
  :<|> noAuthArgH
  :<|> noAuthH
  :<|> noAuthArgH
  :<|> noAuthArgH
  :<|> noAuthArgArgH

noAuthH :: EkadantaApp Html
noAuthH = throwM err401

noAuthArgH :: forall a. a -> EkadantaApp Html
noAuthArgH _ = throwM err401

noAuthArgArgH :: forall a b. a -> b -> EkadantaApp Html
noAuthArgArgH _ _ = throwM err401

-- | Paginated admin edit list
adminListItemsPaginatedH :: Int -> EkadantaApp Html
adminListItemsPaginatedH pgNum = do
  let query = matchAllQ Nothing $ (pgNum - 1) * _DEFAULT_PAGE_COUNT
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config query
  case resourcesResp of
    Left _ ->  throwM ContentLoadFailure
    Right result -> do
      let resources = pullHitsResources result
          resourceTotals = pullAggsKey "counts" result
          postTotal =  getKeyCount (T.pack . show $ BlogPost) resourceTotals
          pageCount = (fromMaybe 0 postTotal)  `div` _DEFAULT_PAGE_COUNT
          tagCounts = pullAggsKey "tags" result
          tagList = tagCounts ^.. folded . traverse . (_Object . ix "key" . _String)
      pure $ adminEditListPage (pageCount, pgNum) resources

-- | Create a new item
adminCreateItemGetH :: EkadantaApp Html
adminCreateItemGetH = pure $ adminEditDetailPage Nothing

adminCreateItemPostH :: Resource -> EkadantaApp Html
adminCreateItemPostH item = do
  config <- asks _getConfig
  result <- liftIO $ indexContent config Nothing item
  case result of
    Left err -> throwM $ getServantErrBody err
    Right _ -> pure $ redirectPage "/admin"


-- | Edit a particular item by its uid
adminUpdateItemGetH :: UUID.UUID -> EkadantaApp Html
adminUpdateItemGetH uid = do
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ getDocument config uid
  case resourcesResp of
    Left _ -> throwM MissingContent
    Right resource -> pure $ adminEditDetailPage (Just resource)


adminUpdateItemPostH :: UUID.UUID -> Resource -> EkadantaApp Html
adminUpdateItemPostH uid item = do
  config <- asks _getConfig
  result <- liftIO $ indexContent config (Just uid) item
  case result of
    Left err -> throwM $ getServantErrBody err
    Right _ -> pure $ redirectPage "/admin"


getServantErrBody :: ClientError -> ServerError
getServantErrBody (ConnectionError _) = err500 { errBody = "connection error" }
getServantErrBody (FailureResponse _ res) =  err500 { errBody = responseBody res}
getServantErrBody (DecodeFailure _ rb) =  err500 { errBody = responseBody rb }
getServantErrBody (UnsupportedContentType _ rb) =  err500 { errBody = responseBody rb}
getServantErrBody (InvalidContentTypeHeader _ ) =  err500 { errBody = "invalid content-type" }


-- | Login APIs
type LoginApi =
  "login" :> Get '[HTML] Html
  :<|> "login"
      :> ReqBody '[FormUrlEncoded] LoginForm
      :> Post '[HTML] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Html)

loginHandlers :: CookieSettings -> JWTSettings -> ServerT LoginApi EkadantaApp
loginHandlers cs jwts = loginGetH :<|> loginPostH cs jwts

loginGetH :: EkadantaApp Html
loginGetH = pure adminLoginPage

-- Here is the login handler
loginPostH :: CookieSettings
           -> JWTSettings
           -> LoginForm
           -> EkadantaApp (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Html)
loginPostH cookieSettings jwtSettings form = do
  config     <- asks _getConfig
  case validateLogin config form of
    Nothing -> throwM err401
    Just usr -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing           -> throwM err401
        Just applyCookies -> return $ applyCookies $ redirectPage "/admin"

validateLogin :: SiteConfig -> LoginForm -> Maybe AdminUser
validateLogin config (LoginForm uname passwd ) =
  if (uname == adminUsername config) && (passwd == adminPasswd config)
    then Just $ AdminUser (adminName config)
    else Nothing


data AdminUser = AdminUser { name :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON AdminUser
instance FromJSON AdminUser
instance ToJWT AdminUser
instance FromJWT AdminUser
