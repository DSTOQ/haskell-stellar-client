{-# LANGUAGE StrictData      #-}

module Stellar.Client.Types
  ( AccountDetails (..)
  , AccountId (..)
  , printAccountId
  , AccountFlags
  , Balance
  , Cursor
  , Order (..)
  , Liabilities
  , TransactionDetails (..)
  ) where

import Control.Monad    (fail)
import Control.Newtype
import Data.Aeson.Types
import Data.Char        (isDigit)
import Protolude
import Stellar
import Web.HttpApiData  (ToHttpApiData (..))

newtype AccountId
  = AccountId PublicKey
  deriving (Eq, Show, FromJSON, ToJSON)

instance Newtype AccountId PublicKey where
  pack = AccountId
  unpack (AccountId pk) = pk

printAccountId :: AccountId -> Text
printAccountId = printPublicKey . unpack


data AccountFlags
  = AccountFlag
   { _authRequired  :: Bool
   , _authRevocable :: Bool
   } deriving (Eq, Show)

instance FromJSON AccountFlags where
  parseJSON = withObject "Account Flag" $ \o -> do
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
    _balance <- Stroop <$> readStellarStupidStringAsInt64 o "balance"
    buying <- readStellarStupidStringAsInt64 o "buying_liabilities"
    selling <- readStellarStupidStringAsInt64 o "selling_liabilities"
    let _liabilities = Liabilities buying selling
    limit <- o .:? "limit"
    _limit <- traverse (maybe (fail "Invalid limit") pure  . readMaybe) limit
    let readCreditAlphanum = AssetCreditAlphanum
          <$> o .: "asset_code"
          <*> o .: "asset_issuer"
    _asset <- o .: "asset_type" >>= \case
      PreciseAssetTypeNative           -> return AssetNative
      PreciseAssetTypeCreditAlphanum4  -> readCreditAlphanum
      PreciseAssetTypeCreditAlphanum12 -> readCreditAlphanum
    return Balance {..}

readStellarStupidStringAsInt64 :: Object -> Text -> Parser Int64
readStellarStupidStringAsInt64 o key = do
  numStr <- o .: key
  maybe (fail ("Invalid " <> toS key)) pure $ readMaybe $ filter isDigit numStr

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


data AccountDetails
  = AccountDetails
  { _id             :: AccountId
  , _publicKey      :: PublicKey
  , _sequenceNumber :: SequenceNumber
  , _subentryCount  :: Word32
  , _thresholds     :: Thresholds
  , _balances       :: [Balance]
  , _flags          :: AccountFlags
  , _signers        :: [Signer]
  , _data           :: Map Text DataValue
  } deriving (Eq, Show, Generic)

instance FromJSON AccountDetails where
  parseJSON = withObject "AccountDetails" $ \o -> do
    _id             <- o .: "id"
    _publicKey      <- o .: "account_id"
    _sequenceNumber <- o .: "sequence"
    _subentryCount  <- o .: "subentry_count"
    _balances       <- o .: "balances"
    _thresholds     <- o .: "thresholds"
    _flags          <- o .: "flags"
    _signers        <- o .: "signers"
    _data           <- o .: "data"
    return AccountDetails {..}


newtype Cursor
  = Cursor Text
  deriving (Eq, Show, FromJSON, ToHttpApiData)

data Order
  = Ascending | Descending
  deriving (Eq, Show)

instance ToHttpApiData Order where
  toUrlPiece = \case
    Ascending -> "asc"
    Descending -> "desc"

newtype TransactionId
  = TransactionId Sha256
  deriving (Eq, Show, FromJSON)


newtype TransactionDetails
  = TransactionDetails
  { _id :: TransactionId
  } deriving (Eq, Show, Generic)

instance FromJSON TransactionDetails where
  parseJSON = withObject "TransactionDetails" $ \o -> do
    _id <- o .: "id"
    return TransactionDetails {..}
