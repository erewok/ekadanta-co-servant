module Site.AdminResources where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.UUID                as UUID
import           GHC.Generics
import           Network.HTTP.Types          (Status, status400, status404)
import           RIO                  hiding ( Handler )
import           RIO.List                    ( headMaybe )
import qualified RIO.HashMap              as HM
import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.HTML.Blaze
import qualified Servant.Checked.Exceptions as SCE
import           Text.Blaze                  (ToMarkup(..), Markup, text)
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
    :<|> "admin" :> "item" :> Capture "item_id" UUID.UUID :> Get '[HTML] Html 

adminHandlers :: AuthResult AdminUser -> ServerT AdminApi EkadantaApp
adminHandlers (Servant.Auth.Server.Authenticated user) =
  adminListItemsPaginatedH 1
  :<|> adminListItemsPaginatedH
  :<|> adminCreateItemH
  :<|> adminUpdateItemH
adminHandlers _ = 
  noAuthH
  :<|> noAuthArgH
  :<|> noAuthH
  :<|> noAuthArgH

noAuthH :: EkadantaApp Html
noAuthH = throwM err401

noAuthArgH :: forall a. a -> EkadantaApp Html
noAuthArgH _ = throwM err401

-- | Paginated admin edit list
adminListItemsPaginatedH :: Int -> EkadantaApp Html
adminListItemsPaginatedH pgNum = do
  let query = matchAllQ Nothing $ (pgNum - 1) * _DEFAULT_PAGE_COUNT
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ searchContent config query
  case resourcesResp of
    Left err ->  throwM ContentLoadFailure
    Right result -> do
      let resources = pullHitsResources result
          resourceTotals = pullAggsKey "counts" result
          postTotal =  getKeyCount (T.pack . show $ BlogPost) resourceTotals
          pageCount = if isJust postTotal then fromJust postTotal else 0
          tagCounts = pullAggsKey "tags" result
          tagList = tagCounts ^.. folded . traverse . (_Object . ix "key" . _String)
      pure $ adminEditListPage (pageCount, pgNum) resources


-- | Create a new item
adminCreateItemH :: EkadantaApp Html
adminCreateItemH = pure $ adminEditDetailPage Nothing


-- | Edit a particular item by its uid
adminUpdateItemH :: UUID.UUID -> EkadantaApp Html
adminUpdateItemH uid = do
  config     <- asks _getConfig
  resourcesResp  <- liftIO $ getDocument config uid
  case resourcesResp of
    Left err -> throwM MissingContent
    Right resource -> pure $ adminEditDetailPage (Just resource)


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
loginPostH _ _ _ = throwM err401

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
