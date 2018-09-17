{-# LANGUAGE StrictData #-}

module Stellar.Client.Types
  ( Account (..)
  , AccountId
  , AccountFlag
  , Balance
  , Liabilities
  ) where

import           Protolude
import           Stellar

newtype AccountId
  = AccountId PublicKey
  deriving (Eq, Show)

data AccountFlag
  = AuthImmutable Bool
  | AuthRequired Bool
  | AuthRevocable Bool
  deriving (Eq, Show)

data Liabilities
  = Liabilities
  { _buying  :: Int64
  , _selling :: Int64
  } deriving (Eq, Show)

data Balance
  = Balance
  { _balance     :: Stroop
  , _liabilities :: Liabilities
  , _limit       :: Maybe Int64
  , _asset       :: Asset
  } deriving (Eq, Show)

data Account
  = Account
  { _id             :: AccountId
  , _publicKey      :: PublicKey
  , _sequenceNumber :: SequenceNumber
  , _subentryCount  :: Word32
  , _balances       :: [Balance]
  , _flags          :: [AccountFlag]
  , _signers        :: [Signer]
  , _data           :: DataValue
  } deriving (Eq, Show)
