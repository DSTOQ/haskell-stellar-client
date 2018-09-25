{-# LANGUAGE DefaultSignatures #-}

module Control.Monad.Stellar
  ( MonadStellar
  , accountDetails
  , accountTransactions
  ) where

import Named
import Protolude
import Stellar.Client.Types

import Control.Monad.Trans        (MonadTrans)
import Control.Monad.Trans.Cont   (ContT)
import Control.Monad.Trans.RWS    (RWST)
import Control.Monad.Trans.Writer (WriterT)

class Monad m => MonadStellar m where
  accountDetails :: AccountId -> m AccountDetails
  default accountDetails
    :: (MonadTrans t, MonadStellar m1, m ~ t m1)
    => AccountId
    -> m AccountDetails
  accountDetails = lift . accountDetails

  accountTransactions
    :: AccountId
    -> "cursor" :? Maybe Cursor
    -> "order"  :? Maybe SortOrder
    -> "limit"  :? Maybe Int
    -> m ([TransactionDetails], Cursor)
  default accountTransactions
    :: (MonadTrans t, MonadStellar m1, m ~ t m1)
    => AccountId
    -> "cursor" :? Maybe Cursor
    -> "order"  :? Maybe SortOrder
    -> "limit"  :? Maybe Int
    -> m ([TransactionDetails], Cursor)
  accountTransactions acc
    (argDef #cursor Nothing -> cursor)
    (argDef #order  Nothing -> order)
    (argDef #limit  Nothing -> limit)
    = lift $ accountTransactions acc
      ! #cursor cursor
      ! #order order
      ! #limit limit

instance MonadStellar m => MonadStellar (ReaderT r m)
instance (MonadStellar m, Monoid w) => MonadStellar (WriterT w m)
instance MonadStellar m => MonadStellar (StateT s m)
instance (MonadStellar m, Monoid w) => MonadStellar (RWST r w s m)
instance MonadStellar m => MonadStellar (ExceptT e m)
instance MonadStellar m => MonadStellar (ContT r m)
