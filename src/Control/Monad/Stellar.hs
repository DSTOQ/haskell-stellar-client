{-# LANGUAGE DefaultSignatures #-}

module Control.Monad.Stellar
  ( MonadStellar
  , account
  ) where

import Control.Monad.Trans        (MonadTrans)
import Control.Monad.Trans.Cont   (ContT)
import Control.Monad.Trans.RWS    (RWST)
import Control.Monad.Trans.Writer (WriterT)
import Protolude
import Stellar.Client.Types


class Monad m => MonadStellar m where
  account :: AccountId -> m Account

  default account :: (MonadTrans t, MonadStellar m1, m ~ t m1)
                  => AccountId -> m Account
  account = lift . account

instance MonadStellar m => MonadStellar (ReaderT r m)
instance (MonadStellar m, Monoid w) => MonadStellar (WriterT w m)
instance MonadStellar m => MonadStellar (StateT s m)
instance (MonadStellar m, Monoid w) => MonadStellar (RWST r w s m)
instance MonadStellar m => MonadStellar (ExceptT e m)
instance MonadStellar m => MonadStellar (ContT r m)
