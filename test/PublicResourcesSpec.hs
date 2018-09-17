module PublicResourcesSpec where

import qualified Control.Concurrent               as C
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.UUID                        as UUID
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           RIO                       hiding ( Handler )
import           Servant
import           System.Log.FastLogger            ( newStdoutLoggerSet 
                                                  , defaultBufSize
                                                  , pushLogStrLn
                                                  , flushLogStr )
import           System.Envy                                                  
import           Test.Hspec
import           Test.Hspec.Wai                                                    
import           Test.Hspec.Wai.Matcher

import           Site
import           Site.Loggers
import           Site.PublicResources
import           Site.Search
import           Site.Types


spec :: Spec
spec = around withElasticsearch $ do
  ctx <- runIO makeCtx
  with (pure $ ekadantaApp ctx) $
    describe "GET /posts" $
      it "should return post-list rendered" $
        get "/posts" `shouldRespondWith` 200


-- | Test environment preparation functions
makeCtx :: IO EkadantaCtx
makeCtx = do
  let config = defConfig :: SiteConfig
  let realConfig = config {
    environment = Test
    , version = "testing"
    , esHost = "http://localhost"
    , esPort = "9999"    
  }
  logset <- newStdoutLoggerSet defaultBufSize
  pure EkadantaCtx { _getLogger = logset, _getConfig = realConfig }

-- | We stub out the Elasticsearch server so we can test behaviors
withElasticsearch :: IO () -> IO ()
withElasticsearch action =
  bracket (liftIO $ C.forkIO $ Warp.run 9999 esTestApp)
    C.killThread
    (const action)


esTestApp :: Application
esTestApp = serve (Proxy :: Proxy SearchAPI) esTestServer

esTestServer :: Server SearchAPI
esTestServer =
  createESIndex :<|> searchESIndex :<|> getESDocument :<|> upsertESDocument

createESIndex :: Value -> Handler Value
createESIndex = undefined

searchESIndex :: Value -> Handler Value
searchESIndex = undefined

getESDocument :: UUID.UUID -> Handler Value
getESDocument = undefined

upsertESDocument :: UUID.UUID -> Resource -> Handler Value
upsertESDocument = undefined