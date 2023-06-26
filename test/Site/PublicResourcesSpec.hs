module Site.PublicResourcesSpec where

import qualified Control.Concurrent               as C
import Control.DeepSeq ()
import           Control.Exception                (evaluate)
import Data.Aeson ( Value(Object, Array, String), ToJSON(toJSON) )
import Data.Aeson.Key                             as Key
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Lens ()
import           Data.Text.Encoding               ( encodeUtf8 )
import qualified Data.UUID                        as UUID
import Network.Wai ( Application )
import qualified Network.Wai.Handler.Warp         as Warp
import RIO
    ( ($),
      Eq((==)),
      Monad((>>)),
      Applicative(pure),
      Semigroup((<>)),
      Bool(True),
      Maybe(Just),
      IO,
      (.),
      MonadIO(liftIO),
      Proxy(..),
      undefined,
      bracket,
      runRIO,
      encodeUtf8 )
import qualified RIO.Vector                    as V
import Servant
    ( type (:<|>)((:<|>)), Handler, Server, UVerb, hoistServer, serve )
import Servant.API.UVerb (Union, WithStatus)
import Servant.Server ()
import           System.Log.FastLogger            ( newStdoutLoggerSet
                                                  , defaultBufSize
                                                  , pushLogStrLn
                                                  , flushLogStr )
import System.Envy ( DefConfig(defConfig) )
import Test.Hspec ( around_, describe, it, runIO, Spec )
import Test.Hspec.Wai ( get, shouldRespondWith, with )
import Test.Hspec.Wai.Matcher ()

import Site
    ( SiteConfig(environment, version, esHost, esPort),
      Environment(Test),
      EkadantaCtx(..) )
import Site.Loggers ()
import Site.PublicResources ( PublicApi, publicHandlers )
import Site.Search ( SearchAPI )
import Site.Types
    ( ContentEncoding(ContentMarkdown),
      ResourceType(BlogPost),
      Resource(..) )


spec :: Spec
spec = around_ withElasticsearch $ do
  ctx <- runIO makeCtx
  with (pure $ publicApp ctx) $ do
    describe "GET /home" $
      it "should have all the /home sections" $ do
        _ <- get "/" `shouldRespondWith` 200
        pure ()

    describe "GET /posts" $
      it "should return post-list rendered" $ do
        _ <- get "/posts" `shouldRespondWith` 200
        pure ()
    describe "GET /posts/2" $
      it "should return post-list page 2 rendered" $ do
        _ <- get "/posts/1" `shouldRespondWith` 200
        pure ()
    describe "GET /posts/<uid>" $ do
      it "should throw 404 when it can't parse the result" $ do
        _ <- get (encodeUtf8 $ "/posts/" <> UUID.toText UUID.nil) `shouldRespondWith` 404
        pure ()
      it "should return a specific post" $ do
        _ <- get "/posts/4102f030-a81c-44fa-9a7e-5ddc247297a7" `shouldRespondWith` 200
        pure ()
    describe "GET /projects/<uid>" $ do
      it "should throw 404 when it can't parse the result" $ do
        _ <- get (encodeUtf8 $ "/projects/" <> UUID.toText UUID.nil) `shouldRespondWith` 404
        pure ()
      it "should return a specific projects" $ do
        _ <- get "/projects/4102f030-a81c-44fa-9a7e-5ddc247297a7" `shouldRespondWith` 200
        pure ()


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
    (\_ -> C.threadDelay 500 >> action)


esTestApp :: Application
esTestApp = serve (Proxy :: Proxy SearchAPI) esTestServer

esTestServer :: Server SearchAPI
esTestServer =
  createESIndex :<|> searchESIndex :<|> getESDocument :<|> upsertESDocument


searchESIndex :: Value -> Handler Value
searchESIndex (Object hmap) = pure $ Object $ fromList [
  (Key.fromText "hits", Object $ fromList [
    (Key.fromText "hits", Array $ V.fromList [Object $ fromList [((Key.fromText "_source"), toJSON defaultPostSource)]] )
    ] )
  ]
searchESIndex _ = pure $ Object $ fromList [((Key.fromText "bad") , String "data")]  -- This is how we signal a failing result


getESDocument :: UUID.UUID -> Handler Value
getESDocument uid =
  if uid == UUID.nil -- This is how we single a failing result...
    then pure . Object $ fromList [((Key.fromText "bad"), String "data")]
    else pure $ Object $ fromList [((Key.fromText "_source"), defaultPostSource)]

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
    , _pid = UUID.fromString "4102f030-a81c-44fa-9a7e-5ddc247297a7"
  }


createESIndex :: Maybe Bool -> Value -> Handler Value
createESIndex = undefined

upsertESDocument :: UUID.UUID -> Resource -> Handler (Union '[WithStatus 201 Value, WithStatus 200 Value])
upsertESDocument = undefined
