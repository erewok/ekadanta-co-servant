module Site (
  hoister
  , ekadantaApp
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

import Site.Config as X
import Site.Exceptions as X
import Site.Loggers as X
import Site.PublicResources as X
import Site.Types as X



hoister :: EkadantaCtx -> EkadantaApp a -> Handler a
hoister = runRIO


ekadantaApp :: EkadantaCtx -> Application
ekadantaApp ctx = 
  serve publicApi $ 
    hoistServer publicApi (hoister ctx) publicHandlers


