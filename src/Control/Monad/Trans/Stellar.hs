{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Stellar
  ( MonadStellar (..)
  , StellarT (..)
  ) where

import           Control.Monad.Catch   (MonadThrow)
import           Control.Monad.Rest    (MonadRest, RelativeRes (..),
                                        emptyRelativeRes)
import qualified Control.Monad.Rest    as Rest
import           Control.Monad.Stellar (MonadStellar, account)
import           Control.Monad.Trans   (MonadTrans)
import           Control.Newtype       (Newtype, pack, unpack)
import           Protolude             hiding (get)
import           Stellar.Client.Types
import qualified Text.URI              as URI


newtype StellarT m a
  = StellarT
  { runStellarT :: m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadIO
             , MonadRest
             )

instance MonadTrans StellarT where
  lift = StellarT

instance Newtype (StellarT m a) (m a) where
  unpack = runStellarT
  pack = StellarT

instance MonadRest m => MonadStellar (StellarT m) where
  account accountId = do
    accounts <- URI.mkPathPiece "accounts"
    accId <- URI.mkPathPiece $ printAccountId accountId
    let path = accounts :| pure accId
    lift $ Rest.get $ emptyRelativeRes { path = Just (False, path) }
