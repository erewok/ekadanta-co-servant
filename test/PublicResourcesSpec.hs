module PublicResourcesSpec where

import qualified Control.Concurrent          as C
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.UUID                   as UUID
import qualified Network.Wai.Handler.Warp    as Warp
import           RIO
import           Servant
import           Test.Hspec.Wai.Matcher

import           Site.PublicResources
import           Site.Search





-- | We stub out the Elasticsearch server so we can test behaviors
withElasticsearch :: IO () -> IO ()
withElasticsearch action =
  bracket (lioftIO $ forkIO $ Warp.run 9999 )


esTestApp :: Warp.Application
esTestApp = server (Proxy :: Proxy SearchAPI) 

esTestServer :: Server SearchAPI
esTestServer =
  createIndex :<|> searchIndex :<|> getDocument :<|> upsertDocument

createIndex :: 