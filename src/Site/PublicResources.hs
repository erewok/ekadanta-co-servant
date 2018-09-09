module Site.PublicResources where

import Data.Text (Text)
import Servant

import Site.Html.Contact
import Site.Html.Home
import Site.Html.ContentDetail
import Site.Html.ContentList


type PublicApi = 
  "posts" :> Get '[Html] Html
  :<|> "posts" :> Capture "post_id" Text :> Get '[Html] Html
  :<|> "search" :> ReqBody '[FormUrlEncoded] Text :> Post '[HTML] Html
  :<|> "about" :> Get '[Html] Html
  :<|> "contact" :> Get '[Html] Html
  :<|> "contact" :> ReqBody '[FormUrlEncoded] ContactForm :> Post '[HTML] Html
  :<|> Get '[Html] Html


publicHandlers :: ServerT PublicApi Handler
publicHandlers =
  getPostListH
  :<|> getPostH
  :<|> searchResultstH
  :<|> aboutH
  :<|> contactH
  :<|> contactPostH
  :<|> homeH


homeH :: Handler Html
homeH = do
  undefined
  -- homePage


contactPostH :: ContactForm -> Handler Html
contactPostH contactF = do
  liftIO $ sendContact contactF
  pure $ redirectPage "/thanks"


publicApi :: Proxy PublicApi
publicApi = Proxy