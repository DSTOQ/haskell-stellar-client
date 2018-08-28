{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}

module Stellar.Types where

import           Control.Monad        (fail)
import           Data.Binary.Extended
import           Data.Binary.Get      (Get)
import           Data.LargeWord       (Word256, Word96)
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Word            (Word32)
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
  } deriving (Eq, Show, Binary)


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


data AssetType
  = AssetTypeNative
  | AssetTypeCreditAlphanum4
  | AssetTypeCreditAlphanum12
  deriving (Eq, Show, Enum, Bounded)

instance Binary AssetType where
  get = getEnum
  put = putEnum

assetType :: Asset -> AssetType
assetType = \case
  AssetNative -> AssetTypeNative
  AssetCreditAlphanum4 _ _ -> AssetTypeCreditAlphanum4
  AssetCreditAlphanum12 _ _ -> AssetTypeCreditAlphanum12

data Asset
  = AssetNative
  | AssetCreditAlphanum4 AssetCode4 PublicKey
  | AssetCreditAlphanum12 AssetCode12 PublicKey
  deriving (Eq, Show)

instance Binary Asset where
  get = get >>= \case
    AssetTypeNative           -> pure AssetNative
    AssetTypeCreditAlphanum4  -> AssetCreditAlphanum4 <$> get <*> get
    AssetTypeCreditAlphanum12 -> AssetCreditAlphanum12 <$> get <*> get
  put AssetNative = put AssetTypeNative
  put (AssetCreditAlphanum4 code pk) =  put AssetTypeCreditAlphanum4  >> put code >> put pk
  put (AssetCreditAlphanum12 code pk) = put AssetTypeCreditAlphanum12 >> put code >> put pk


data Price
  = Price
  { numerator   :: Int32
  , denominator :: Int32
  } deriving (Eq, Show, Generic)

instance Binary Price


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
  | MemoText (Limited 28 ByteString)
  | MemoId Word64
  | MemoHash Hash -- the hash of what to pull from the content server
  | MemoReturn Hash -- the hash of the tx you are rejecting
  deriving (Eq, Show)

instance Binary Memo where
  put MemoNone       = put MemoTypeNone
  put (MemoText t)   = put MemoTypeText >> put t
  put (MemoId i)     = put MemoTypeId >> put i
  put (MemoHash h)   = put MemoTypeHash >> put h
  put (MemoReturn h) = put MemoTypeReturn >> put h
  get = do
    t <- get
    case t of
      MemoTypeNone   -> pure MemoNone
      MemoTypeText   -> MemoText <$> get
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
  } deriving (Eq, Show, Generic)

instance Binary ManageOfferOp


data CreatePassiveOfferOp
  = CreatePassiveOfferOp
  { selling :: Asset
  , buying  :: Asset
  , amount  :: Int64    -- amount taker gets. if set to 0, delete the offer
  , price   :: Price    -- price of thing being sold in terms of what you are buying
  } deriving (Eq, Show, Generic)

instance Binary CreatePassiveOfferOp


newtype HomeDomain
  = HomeDomain
  { unHomeDomain :: Limited 32 Text
  } deriving (Eq, Show, Binary)


data SetOptionsOp
  = SetOptionsOp
  { inflationDest   :: Maybe PublicKey    -- inflation destination
  , clearFlags      :: Maybe Word32       -- which flags to clear
  , setFlags        :: Maybe Word32       -- which flags to set
  , masterWeight    :: Maybe Word32       -- weight of the master account
  , lowThreshold    :: Maybe Threshold
  , mediumThreshold :: Maybe Threshold
  , highThreshold   :: Maybe Threshold
  , homeDomain      :: Maybe HomeDomain
  , signer          :: Maybe Signer
  } deriving (Eq, Show)

instance Binary SetOptionsOp where
  put op = do
    op & put . Padded . inflationDest
    op & put . Padded . clearFlags
    op & put . Padded . setFlags
    op & put . Padded . masterWeight
    op & put . Padded . lowThreshold
    op & put . Padded . mediumThreshold
    op & put . Padded . highThreshold
    op & put . Padded . homeDomain
    op & put . Padded . signer
  get = SetOptionsOp
    <$> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get
    <*> fmap unPadded get


data ChangeTrustOp
  = ChangeTrustOp
  { line  :: Asset
  , limit :: Maybe Int64   -- limit, Nothing deletes the trust line
  } deriving (Eq, Show)

instance Binary ChangeTrustOp where
  put op = do
    op & put . line
    op & put . Padded . limit
  get = ChangeTrustOp <$> get <*> fmap unPadded get


data AllowTrustOp
  = AllowTrustOp
  { trustor   :: PublicKey
  , asset     :: Either AssetCode4 AssetCode12
  , authorize :: Bool
  } deriving (Eq, Show)

instance Binary AllowTrustOp where
  put op = do
    op & put . trustor
    either (put . (AssetTypeCreditAlphanum4,))
           (put . (AssetTypeCreditAlphanum12,))
           $ asset (op :: AllowTrustOp)
    op & put . Padded . authorize
  get = do
    trustor <- get
    asset <- get >>= \case
      AssetTypeNative -> fail "Can't allow trust for a native asset"
      AssetTypeCreditAlphanum4  -> fmap Left  (get :: Get AssetCode4)
      AssetTypeCreditAlphanum12 -> fmap Right (get :: Get AssetCode12)
    authorize <- fmap unPadded get
    pure $ AllowTrustOp trustor asset authorize


newtype DataValue
  = DataValue (Limited 64 ByteString)
  deriving (Eq, Show, Binary)

unDataValue :: DataValue -> ByteString
unDataValue (DataValue l) = unLimited l


data ManageDataOp
  = ManageDataOp
  { dataName  :: Limited 64 Text
  , dataValue :: Maybe DataValue
  } deriving (Eq, Show)

instance Binary ManageDataOp where
  put op = do
    op & put . dataName
    op & put . Padded . dataValue
  get = ManageDataOp <$> get <*> fmap unPadded get


data OperationType
  = OperationTypeCreateAccount
  | OperationTypePayment
  | OperationTypePathPayment
  | OperationTypeManageOffer
  | OperationTypeCreatePassiveOffer
  | OperationTypeSetOptions
  | OperationTypeChangeTrust
  | OperationTypeAllowTrust
  | OperationTypeAccountMerge
  | OperationTypeInflation
  | OperationTypeManageData
  | OperationTypeBumpSequence
  deriving (Eq, Show, Enum, Bounded)

instance Binary OperationType where
  get = getEnum
  put = putEnum


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
  | ManageData ManageDataOp
  | BumpSequence SequenceNumber
  deriving (Eq, Show)

operationType :: Operation -> OperationType
operationType op = case op of
  CreateAccount _      -> OperationTypeCreateAccount
  Payment _            -> OperationTypePayment
  PathPayment _        -> OperationTypePathPayment
  ManageOffer _        -> OperationTypeManageOffer
  CreatePassiveOffer _ -> OperationTypeCreatePassiveOffer
  SetOptions _         -> OperationTypeSetOptions
  ChangeTrust _        -> OperationTypeChangeTrust
  AllowTrust _         -> OperationTypeAllowTrust
  AccountMerge _       -> OperationTypeAccountMerge
  Inflation            -> OperationTypeInflation
  ManageData _         -> OperationTypeManageData
  BumpSequence _       -> OperationTypeBumpSequence


instance Binary Operation where
  put operation = do
    operation & put . operationType
    case operation of
      CreateAccount op      -> put op
      Payment op            -> put op
      PathPayment op        -> put op
      ManageOffer op        -> put op
      CreatePassiveOffer op -> put op
      SetOptions op         -> put op
      ChangeTrust op        -> put op
      AllowTrust op         -> put op
      AccountMerge pk       -> put pk
      Inflation             -> pure ()
      ManageData op         -> put op
      BumpSequence sn       -> put sn
  get = get >>= \case
    OperationTypeCreateAccount      -> CreateAccount      <$> get
    OperationTypePayment            -> Payment            <$> get
    OperationTypePathPayment        -> PathPayment        <$> get
    OperationTypeManageOffer        -> ManageOffer        <$> get
    OperationTypeCreatePassiveOffer -> CreatePassiveOffer <$> get
    OperationTypeSetOptions         -> SetOptions         <$> get
    OperationTypeChangeTrust        -> ChangeTrust        <$> get
    OperationTypeAllowTrust         -> AllowTrust         <$> get
    OperationTypeAccountMerge       -> AccountMerge       <$> get
    OperationTypeManageData         -> ManageData         <$> get
    OperationTypeBumpSequence       -> BumpSequence       <$> get
    OperationTypeInflation          -> pure Inflation


data AccountOperation
  = AccountOperation
  { sourceAccount :: Maybe PublicKey
  , operation     :: Operation
  } deriving (Eq, Show)

instance Binary AccountOperation where
  put op = do
    put $ Padded $ sourceAccount (op :: AccountOperation)
    put $ operation op
  get = AccountOperation <$> fmap unPadded get <*> get


data Transaction
  = Transaction
  { sourceAccount :: PublicKey
  , fee           :: Fee
  , seqNum        :: SequenceNumber
  , timeBounds    :: Maybe TimeBounds
  , memo          :: Memo
  , operations    :: NonEmpty AccountOperation -- max 100
  } deriving (Eq, Show)

instance Binary Transaction where
  put tx = do
    put $ sourceAccount (tx :: Transaction)
    tx & put . fee
    tx & put . seqNum
    tx & put . Padded . timeBounds
    tx & put . memo
    tx & put . operations
  get = Transaction
    <$> get
    <*> get
    <*> get
    <*> fmap unPadded get
    <*> get
    <*> get


newtype SignatureHint
  = SignatureHint
  { unSignatureHint :: Word32
  } deriving (Eq, Show, Binary)


newtype Signature
  = Signature
  { unSignature :: Limited 256 ByteString -- TODO exact size
  } deriving (Eq, Show, Binary)


data DecoratedSignature
  = DecoratedSignature
  { signatureHint :: SignatureHint
  , signature     :: Signature
  } deriving (Eq, Show, Generic)

instance Binary DecoratedSignature


data TransactionEnvelope
  = TransactionEnvelope
  { transaction :: Transaction
  , signatures  :: [DecoratedSignature]
  } deriving (Eq, Show, Generic)

instance Binary TransactionEnvelope
