module PublicResourcesSpec where

import qualified Control.Concurrent               as C
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text.Encoding               ( encodeUtf8 )
import qualified Data.UUID                        as UUID
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp
import           RIO                       hiding ( Handler )
import qualified RIO.HashMap                   as HM
import qualified RIO.Vector                    as V
import           Servant
import           Servant.Server
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
spec = around_ withElasticsearch $ do
  ctx <- runIO makeCtx
  with (pure $ publicApp ctx) $ do
    describe "GET /home" $
      it "should have all the /home sections" $
        get "/" `shouldRespondWith` 200
    describe "GET /posts" $
      it "should return post-list rendered" $
        get "/posts" `shouldRespondWith` 200
    describe "GET /posts/2" $
      it "should return post-list page 2 rendered" $
        get "/posts/1" `shouldRespondWith` 200
    describe "GET /posts/<uid>" $ do
      it "should throw 404 when it can't parse the result" $
        get (encodeUtf8 $ "/posts/" <> UUID.toText UUID.nil) `shouldRespondWith` 404
      it "should return a specific post" $
        get "/posts/4102f030-a81c-44fa-9a7e-5ddc247297a7" `shouldRespondWith` 200


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


publicApp :: EkadantaCtx -> Application
publicApp ctx = 
  serve (Proxy :: Proxy PublicApi) $ 
    hoistServer (Proxy :: Proxy PublicApi) (runRIO ctx) publicHandlers


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


searchESIndex :: Value -> Handler Value
searchESIndex (Object hmap) = pure $ Object $ HM.fromList [
  ("hits", Object $ HM.fromList [ 
    ("hits", Array $ V.fromList [Object $ HM.fromList [("_source", toJSON defaultPostSource)]] )
    ] )
  ]
searchESIndex _ = pure $ Object $ HM.fromList [("bad", String "data")]  -- This is how we signal a failing result


getESDocument :: UUID.UUID -> Handler Value
getESDocument uid = 
  if uid == UUID.nil -- This is how we single a failing result...
    then pure . Object $ HM.fromList [("bad", String "data")]
    else pure $ Object $ HM.fromList [("_source", defaultPostSource)]
  
defaultPostSource :: Value
defaultPostSource = 
  toJSON $ Resource {
    _pubdate = "2018-01-01"
    , _resourceType = BlogPost
    , _contentEncoding = ContentMarkdown
    , _featuredImage = Just "http://some-image" 
    , _published = True
    , _body = "This is a post body"
    , _title = "Post Title"
    , _lede = "Some posts have some teaser text"
    , _tags = ["testing", "defaults", "elasticsearch"]
    , _pid = "4102f030-a81c-44fa-9a7e-5ddc247297a7"
  }


createESIndex :: Value -> Handler Value
createESIndex = undefined

upsertESDocument :: UUID.UUID -> Resource -> Handler Value
upsertESDocument = undefined
