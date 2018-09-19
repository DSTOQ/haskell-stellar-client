{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Stellar
  ( MonadStellar (..)
  , StellarT (..)
  ) where

import           Control.Monad.Catch            (MonadThrow)
import           Control.Monad.Rest             (MonadRest, RelativeRes (..),
                                                 emptyRelativeRes)
import qualified Control.Monad.Rest             as Rest
import           Control.Monad.Stellar          (MonadStellar, account)
import           Control.Monad.Trans            (MonadTrans)
import           Control.Monad.Trans.Lift.Catch (LiftCatch, liftCatch)
import           Control.Monad.Trans.Lift.StT   (StT)
import           Control.Monad.Trans.Rest       (RestT)
import           Control.Newtype                (Newtype, pack, unpack)
import           Protolude                      hiding (get)
import           Stellar.Client.Types
import qualified Text.URI                       as URI


newtype StellarT m a
  = StellarT
  { runStellarT :: RestT m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadTrans
             , MonadThrow
             , MonadIO
             , LiftCatch
             , MonadRest
             )

type instance StT StellarT a = a

instance Newtype (StellarT m a) (RestT m a) where
  unpack = runStellarT
  pack = StellarT

instance MonadError e m => MonadError e (StellarT m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadRest m => MonadStellar (StellarT m) where
  account accountId = do
    accounts <- URI.mkPathPiece "accounts"
    accId <- URI.mkPathPiece $ printAccountId accountId
    let path = accounts :| pure accId
    lift $ Rest.get $ emptyRelativeRes { path = Just (False, path) }
