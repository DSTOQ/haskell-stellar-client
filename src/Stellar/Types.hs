{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}

module Stellar.Types where

import           Data.Binary.Extended
import           Data.Binary.Get      (Get)
import           Data.LargeWord       (Word256, Word96)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Word            (Word32)
import           Prelude              (String)
import           Protolude            hiding (get, put)

data PublicKeyType
  = PublicKeyTypeEd25519
  deriving (Eq, Show, Enum, Bounded)

instance Binary PublicKeyType where
  get = getEnum
  put = putEnum


newtype PublicKey
  = PublicKeyEd25519
  { unPublicKeyEd25519 :: Word256
  } deriving (Eq, Show)

instance Binary PublicKey where
  get = (get :: Get PublicKeyType) >> fmap PublicKeyEd25519 get
  put k = put PublicKeyTypeEd25519 >> put (unPublicKeyEd25519 k)


data SignerKeyType
  = SignerKeyTypeEd25519
  | SignerKeyTypePreAuthTx
  | SignerKeyTypeHashX
  deriving (Eq, Show, Enum, Bounded)

instance Binary SignerKeyType where
  get = getEnum
  put = putEnum


data SignerKey
  = SignerKeyEd25519 Word256
  | SignerKeyPreAuthTx Word256
  | SignerKeyHashX Word256
  deriving (Eq, Show)

instance Binary SignerKey where
  get = do
    kt :: SignerKeyType <- get
    get <&> case kt of
      SignerKeyTypeEd25519   -> SignerKeyEd25519
      SignerKeyTypePreAuthTx -> SignerKeyPreAuthTx
      SignerKeyTypeHashX     -> SignerKeyHashX
  put (SignerKeyEd25519 w256)   = put SignerKeyTypeEd25519   >> put w256
  put (SignerKeyPreAuthTx w256) = put SignerKeyTypePreAuthTx >> put w256
  put (SignerKeyHashX w256)     = put SignerKeyTypeHashX     >> put w256


newtype Threshold
  = Threshold
  { unThreshold :: Word32
  } deriving (Eq, Show, Binary)

newtype AssetCode4
  = AssetCode4
  { unAssetCode4 :: Word32
  } deriving (Eq, Show, Binary)

newtype AssetCode12
  = AssetCode12
  { unAssetCode12 :: Word96
  } deriving (Eq, Show, Binary)


newtype DataValue
  = DataValue
  { unDataValue :: ByteString
  } deriving (Eq, Show)

instance Binary DataValue where
  get = DataValue <$> fmap unPadded get
  put = put . Padded . unDataValue


data AssetType
  = AssetTypeNative
  | AssetTypeCreditAlphanum4
  | AssetTypeCreditAlphanum12
  deriving (Eq, Show, Enum, Bounded)

instance Binary AssetType where
  get = getEnum
  put = putEnum

data Asset
  = AssetNative
  | AssetCreditAlphanum4 AssetCode4 PublicKey
  | AssetCreditAlphanum12 AssetCode12 PublicKey
  deriving (Eq, Show)

instance Binary Asset where
  get = do
    kt :: AssetType <- get
    case kt of
      AssetTypeNative           -> pure AssetNative
      AssetTypeCreditAlphanum4  -> AssetCreditAlphanum4 <$> get <*> get
      AssetTypeCreditAlphanum12 -> AssetCreditAlphanum12 <$> get <*> get
  put AssetNative =
    put AssetTypeNative
  put (AssetCreditAlphanum4 code pk) =
    put AssetTypeCreditAlphanum4 >> put code >> put pk
  put (AssetCreditAlphanum12 code pk) =
    put AssetTypeCreditAlphanum12 >> put code >> put pk


data Price
  = Price
  { numerator   :: Int32
  , denominator :: Int32
  } deriving (Eq, Show)

instance Binary Price where
  get = Price <$> get <*> get
  put (Price n d) = put n >> put d


newtype Fee
  = Fee
  { unFee :: Word32
  } deriving (Eq, Show, Binary)


newtype SequenceNumber
  = SequenceNumber
  { unSequenceNumber :: Int64
  } deriving (Eq, Show, Binary)


data TimeBounds
  = TimeBounds
  { timeBoundsMinTime :: Word64
  , timeBoundsMaxTime :: Maybe Word64 -- 0 here means no maxTime
  } deriving (Eq, Show)

instance Binary TimeBounds where
  get = TimeBounds <$> get <*> fmap unPadded get
  put (TimeBounds mn mx) = put mn >> put (Padded mx)

newtype Hash
  = Hash
  { unHash :: Word256
  } deriving (Eq, Show, Binary)


data MemoType
  = MemoTypeNone
  | MemoTypeText
  | MemoTypeId
  | MemoTypeHash
  | MemoTypeReturn
  deriving (Eq, Show, Enum, Bounded)

instance Binary MemoType where
  get = getEnum
  put = putEnum


data Memo
  = MemoNone
  | MemoText Text -- string text<28>;
  | MemoId Word64
  | MemoHash Hash -- the hash of what to pull from the content server
  | MemoReturn Hash -- the hash of the tx you are rejecting
  deriving (Eq, Show)

instance Binary Memo where
  put MemoNone       = put MemoTypeNone
  put (MemoText t)   = put MemoTypeText >> put (Padded t)
  put (MemoId i)     = put MemoTypeId >> put i
  put (MemoHash h)   = put MemoTypeHash >> put h
  put (MemoReturn h) = put MemoTypeReturn >> put h
  get = do
    t <- get
    case t of
      MemoTypeNone   -> pure MemoNone
      MemoTypeText   -> MemoText . unPadded <$> get
      MemoTypeId     -> MemoId <$> get
      MemoTypeHash   -> MemoHash <$> get
      MemoTypeReturn -> MemoReturn <$> get


data Signer
  = Signer
  { key    :: SignerKey
  , weight :: Word32
  } deriving (Eq, Show, Generic)

instance Binary Signer


data CreateAccountOp
  = CreateAccountOp
  { destination     :: PublicKey
  , startingBalance :: Int64
  } deriving (Eq, Show, Generic)

instance Binary CreateAccountOp


data PaymentOp
  = PaymentOp
  { destination :: PublicKey -- recipient of the payment
  , asset       :: Asset     -- what they end up with
  , amount      :: Int64     -- amount they end up with
  } deriving (Eq, Show, Generic)

instance Binary PaymentOp


data PathPaymentOp
  = PathPaymentOp
  { sendAsset   :: Asset      -- asset we pay with
  , sendMax     :: Int64      -- the maximum amount of sendAsset to send (excluding fees).
  , destination :: PublicKey  -- recipient of the payment
  , destAsset   :: Asset      -- what they end up with
  , destAmount  :: Int64      -- amount they end up with
  , path        :: [Asset]    -- additional hops it must go through to get there
  } deriving (Eq, Show, Generic)

instance Binary PathPaymentOp


newtype OfferId
  = OfferId
  { unOfferId :: Word64
  } deriving (Eq, Show, Binary)


data ManageOfferOp
  = ManageOfferOp
  { selling :: Asset
  , buying  :: Asset
  , amount  :: Int64
  , price   :: Price    -- price of thing being sold in terms of what you are buying
  , offerId :: OfferId
  } deriving (Eq, Show)

data CreatePassiveOfferOp
  = CreatePassiveOfferOp
  { selling :: Asset
  , buying  :: Asset
  , amount  :: Int64    -- amount taker gets. if set to 0, delete the offer
  , price   :: Price    -- price of thing being sold in terms of what you are buying
  } deriving (Eq, Show)

data SetOptionsOp
  = SetOptionsOp
  { inflationDest   :: Maybe PublicKey    -- inflation destination
  , clearFlags      :: Maybe Word32       -- which flags to clear
  , setFlags        :: Maybe Word32       -- which flags to set
  , masterWeight    :: Maybe Word32       -- weight of the master account
  , lowThreshold    :: Maybe Threshold
  , mediumThreshold :: Maybe Threshold
  , highThreshold   :: Maybe Threshold
  , homeDomain      :: Maybe String
  , signer          :: Maybe Signer
  } deriving (Eq, Show)

data ChangeTrustOp
  = ChangeTrustOp
  { line  :: Asset
  , limit :: Maybe Int64   -- limit, Nothing deletes the trust line
  } deriving (Eq, Show)

data AllowTrustOp
  = AllowTrustOp
  { trustor   :: PublicKey
  , asset     :: Either AssetCode4 AssetCode12
  , authorize :: Bool
  } deriving (Eq, Show)


data Operation
  = CreateAccount CreateAccountOp
  | Payment PaymentOp
  | PathPayment PathPaymentOp
  | ManageOffer ManageOfferOp
  | CreatePassiveOffer CreatePassiveOfferOp
  | SetOptions SetOptionsOp
  | ChangeTrust ChangeTrustOp
  | AllowTrust AllowTrustOp
  | AccountMerge PublicKey
  | Inflation
  | ManageData String (Maybe DataValue)
  | BumpSequence SequenceNumber
  deriving (Eq, Show)

data AccountOperation
  = AccountOperation
  { accountOperationSourceAccount :: Maybe PublicKey
  , accountOperationOperation     :: Operation
  } deriving (Eq, Show)

data Transaction
  = Transaction
  { transactionSourceAccount :: PublicKey
  , transactionFee           :: Fee
  , transactionSeqNum        :: SequenceNumber
  , transactionTimeBounds    :: Maybe TimeBounds
  , transactionMemo          :: Memo
  , transactionOperations    :: NonEmpty AccountOperation -- max 100
  } deriving (Eq, Show)

newtype SignatureHint
  = SignatureHint
  { unSignatureHint :: Word32
  } deriving (Eq, Show)

newtype Signature
  = Signature
  { unSignature :: ByteString
  } deriving (Eq, Show)

data DecoratedSignature
  = DecoratedSignature
  { decoratedSignatureHint      :: SignatureHint
  , decoratedSignatureSignature :: Signature
  } deriving (Eq, Show)

data TransactionEnvelope
  = TransactionEnvelope
  { transactionEnvelopeTransaction :: Transaction
  , transactionEnvelopeSignatures  :: [DecoratedSignature]
  } deriving (Eq, Show)
