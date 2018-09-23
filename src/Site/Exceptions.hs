{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Site.Exceptions where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Identity         (Identity(Identity, runIdentity))
import qualified Data.Text                  as T
import           Data.WorldPeace               ( IsMember, OpenUnion )
import           Data.WorldPeace.Union
import           Network.HTTP.Types
import           RIO
import qualified Servant.Checked.Exceptions as SCE
import           Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus))
import           Text.Blaze                   (ToMarkup(..), Markup, text)

-- | Errors
-- These errors represent missing content (404s) and failing to retrieve
-- content from the Elasticsearch server due to JSON decoding or other issues (400s)
data AppErrors = MissingContent
               | ContentLoadFailure
              deriving (Eq, Read, Show)

instance Exception AppErrors 

instance ToJSON AppErrors where
  toJSON :: AppErrors -> Value
  toJSON = toJSON . show

instance FromJSON AppErrors where
  parseJSON :: Value -> Parser AppErrors
  parseJSON (String "MissingContent") = pure MissingContent
  parseJSON (String "ContentLoadFailure") = pure ContentLoadFailure
  parseJSON _ = fail "could not parse as MissingContent"

instance ToMarkup AppErrors where
  toMarkup = text . T.pack . show

instance ErrStatus AppErrors where
  toErrStatus :: AppErrors -> Status
  toErrStatus MissingContent = status404
  toErrStatus ContentLoadFailure = status400
