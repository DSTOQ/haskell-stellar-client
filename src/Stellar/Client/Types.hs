{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}

module Stellar.Client.Types
  ( Account (..)
  , AccountId (..)
  , printAccountId
  , AccountFlag
  , Balance
  , Liabilities
  ) where

import Control.Monad    (fail)
import Control.Newtype
import Data.Aeson.Types
import Protolude
import Stellar

newtype AccountId
  = AccountId PublicKey
  deriving (Eq, Show, FromJSON, ToJSON)

instance Newtype AccountId PublicKey where
  pack = AccountId
  unpack (AccountId pk) = pk

printAccountId :: AccountId -> Text
printAccountId = printPublicKey . unpack


data AccountFlag
  = AccountFlag
   { _authImmutable :: Bool
   , _authRequired  :: Bool
   , _authRevocable :: Bool
   } deriving (Eq, Show)

instance FromJSON AccountFlag where
  parseJSON = withObject "Account Flag" $ \o -> do
    _authImmutable <- o .: "auth_immutable"
    _authRequired  <- o .: "auth_required"
    _authRevocable <- o .: "auth_revocable"
    return $ AccountFlag {..}


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

instance FromJSON Balance where
  parseJSON = withObject "Balance" $ \o -> do
    _balance <- o .: "balance"
    buying <- readJsonString o "buying_liabilities"
    selling <- readJsonString o "selling_liabilities"
    let _liabilities = Liabilities buying selling
    _limit <- o .: "limit"
    let readCreditAlphanum = AssetCreditAlphanum
          <$> o .: "asset_code"
          <*> o .: "asset_issuer"
    _asset <- o .: "asset_type" >>= \case
      PreciseAssetTypeNative           -> return AssetNative
      PreciseAssetTypeCreditAlphanum4  -> readCreditAlphanum
      PreciseAssetTypeCreditAlphanum12 -> readCreditAlphanum
    return Balance {..}

readJsonString :: Read a => Object -> Text -> Parser a
readJsonString o key = do
  str <- o .: key
  maybe (fail ("Invalid " <> toS key)) pure (readMaybe str)


data Thresholds
  = Thresholds
  { _lowThreshold    :: Threshold
  , _mediumThreshold :: Threshold
  , _highThreshold   :: Threshold
  } deriving (Eq, Show)

instance FromJSON Thresholds where
  parseJSON = withObject "Thresholds" $ \o -> Thresholds
    <$> o .: "low_threshold"
    <*> o .: "med_threshold"
    <*> o .: "high_threshold"


data Account
  = Account
  { _id             :: AccountId
  , _publicKey      :: PublicKey
  , _sequenceNumber :: SequenceNumber
  , _subentryCount  :: Word32
  , _thresholds     :: Thresholds
  , _balances       :: [Balance]
  , _flags          :: [AccountFlag]
  , _signers        :: [Signer]
  , _data           :: DataValue
  } deriving (Eq, Show, Generic)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o -> do
    _id             <- o .: "id"
    _publicKey      <- o .: "account_id"
    _sequenceNumber <- o .: "sequence"
    _subentryCount  <- o .: "subentry_count"
    _balances       <- o .: "balances"
    _thresholds     <- o .: "thresholds"
    _flags          <- o .: "flags"
    _signers        <- o .: "signers"
    _data           <- o .: "data"
    return Account {..}
