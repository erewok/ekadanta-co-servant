module Site.Search where

import           Database.V5.Bloodhound
import           Database.V5.Bloodhound.Client
import           Database.V5.Bloodhound.Internal.Client  ( BHEnv(..) )
import qualified Data.UUID                               as UUID
import qualified Data.UUID.V4                            as UUID4
import           Network.HTTP.Client
import           RIO

import           Site.Config
import           Site.Types



myIndex :: IndexName
myIndex = IndexName "ekadanta"

myMapping :: MappingName
myMapping = MappingName "content"


constructBHEnv :: (MonadIO m) => SiteConfig -> m BHEnv
constructBHEnv config = do
  let bhServer = esServer config
      bhRequestHook = return
  bhManager <- liftIO $ newManager defaultManagerSettings
  pure BHEnv{..}

runEsTransaction :: (MonadIO m) => SiteConfig -> BH m a -> m a
runEsTransaction config esReq = constructBHEnv config >>= flip runBH esReq

indexDoc :: (MonadBH m) => SiteConfig -> ResourceType -> m Reply
indexDoc config item = do
  uid <- liftIO $ DocId <$> (UUID.toText <$> UUID4.nextRandom)
  runEsTransaction config $ 
    indexDocument myIndex myMapping defaultIndexDocumentSettings item uid
  

getDoc :: (MonadBH m) => SiteConfig -> Text -> m Reply
getDoc config docId = do
  runEsTransaction config $ 
    getDocument myIndex myMapping (DocId docId)