{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Site.Exceptions where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Identity         (Identity(Identity, runIdentity))
import qualified Data.Text                  as T
import           Data.WorldPeace               ( IsMember, OpenUnion )
import           Data.WorldPeace.Union
import           Network.HTTP.Types
import           RIO
import           Servant.Checked.Exceptions
import           Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus))
import           Text.Blaze                   (ToMarkup(..), Markup, text)

-- | Errors
-- These errors represent missing content (404s) and failing to retrieve
-- content from the Elasticsearch server due to JSON decoding or other issues (400s)
data AppErrors = MissingContent
               | ContentLoadFailure
               | UnknownError
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

instance (ToMarkup (OpenUnion es), ToMarkup a) => ToMarkup (Envelope es a) where
  toMarkup :: Envelope es a -> Markup
  toMarkup (ErrEnvelope es) = toMarkup es
  toMarkup (SuccEnvelope a) = toMarkup a

instance ToMarkup (OpenUnion '[AppErrors]) where
  toMarkup someErrors = toMarkup $ openUnion (const UnknownError) id someErrors
