{-# LANGUAGE DefaultSignatures #-}

module Control.Monad.Rest
  ( MonadRest (..)
  , ApiBase (..)
  , RelativeRes (..)
  , emptyRelativeRes
  , resourceUri
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (MonadTrans)
import Data.Aeson.Types
import Protolude           hiding (get)
import Text.URI


data ApiBase
  = ApiBase
  { scheme    :: Maybe (RText 'Scheme)
  , authority :: Either Bool Authority
  } deriving (Eq, Show)

data RelativeRes
  = RelativeRes
  { path     :: Maybe (Bool, NonEmpty (RText 'PathPiece))
  , query    :: [QueryParam]
  , fragment :: Maybe (RText 'Fragment)
  } deriving (Eq, Show)

emptyRelativeRes :: RelativeRes
emptyRelativeRes = RelativeRes Nothing mempty Nothing

resourceUri :: ApiBase -> RelativeRes -> URI
resourceUri b r = URI (scheme b) (authority b) (path r) (query r) (fragment r)

class MonadThrow m => MonadRest m where
  get :: FromJSON v => RelativeRes -> m v

  default get :: ( FromJSON v
                 , MonadTrans t
                 , MonadRest m1
                 , m ~ t m1
                 ) => RelativeRes -> m v
  get = lift . get

instance MonadRest m => MonadRest (ReaderT r m)
instance MonadRest m => MonadRest (ExceptT e m)
