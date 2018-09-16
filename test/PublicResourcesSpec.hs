module PublicResourcesSpec where

import qualified Control.Concurrent          as C
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.UUID                   as UUID
import qualified Network.Wai.Handler.Warp    as Warp
import           RIO
import           Servant
import           Test.Hspec.Wai.Matcher

import           Site
import           Site.PublicResources
import           Site.Search


-- spec :: Spec
-- spec = around withElasticsearch $ do
--   ctx <- runIO makeCtx


-- -- | Test environment preparation functions
-- makeCtx :: IO EkadantaCtx
-- makeCtx = do
--   let config = defConfig :: SiteConfig
--   let realConfig = config {
--     environment = Test

--   }


-- | We stub out the Elasticsearch server so we can test behaviors
withElasticsearch :: IO () -> IO ()
withElasticsearch action =
  C.bracket (liftIO $ C.forkIO $ Warp.run 9999 esTestApp)
    C.killThread
    (const action)


esTestApp :: Warp.Application
esTestApp = server (Proxy :: Proxy SearchAPI) 

esTestServer :: Server SearchAPI
esTestServer =
  createIndex :<|> searchIndex :<|> getDocument :<|> upsertDocument

createIndex :: Value -> Handler Value
createIndex = undefined

searchIndex :: Value -> Handler Value
searchIndex = undefined

getDocument :: UUID.UUID -> Handler Value
getDocument = undefined

upsertDocument :: UUID.UUID -> Resource -> Handler Value
upsertDocument = undefined