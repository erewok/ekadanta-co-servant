module Site (
  ekadantaApp
  , jsonRequestLogger
  , AppErrors(..)
  , EkadantaApp(..)
  , EkadantaCtx(..)
  , Environment(..)
  , SiteConfig(..)
  , LogMessage(..)
) where

import Network.Wai (Application)
import RIO hiding ( Handler )
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

import Site.Config as X
import Site.Exceptions as X
import Site.Loggers as X
import Site.AdminResources as X
import Site.PublicResources as X
import Site.Types as X



type SiteWideApi auths = 
  PublicApi 
  :<|> AdminAndLogin auths
  :<|> "static" :> Raw

siteWideApi :: Proxy (SiteWideApi '[Cookie])
siteWideApi = Proxy

siteWideHandlers :: CookieSettings -> JWTSettings -> ServerT (SiteWideApi auths) EkadantaApp
siteWideHandlers cs jwts =
  publicHandlers
  :<|> adminServer cs jwts
  :<|> serveDirectoryFileServer "static"


ekadantaApp :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> EkadantaCtx -> Application
ekadantaApp cfg cs jwts ctx = 
  serveWithContext siteWideApi cfg $ 
    hoistServerWithContext siteWideApi (Proxy :: Proxy '[CookieSettings, JWTSettings]) (runRIO ctx) (siteWideHandlers cs jwts)
