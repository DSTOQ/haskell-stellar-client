module Stellar.Types where

import           Data.LargeWord     (Word256, Word96)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Word          (Word32)
import           Prelude            (String)
import           Protolude

newtype PublicKey
  = PublicKeyEd25519
  { unPublicKeyEd25519 :: Word256
  } deriving (Eq, Show)

data SignerKey
  = SignerKeyEd25519 Word256
  | SignerKeyPreAuthTx Word256
  | SignerKeyHashX Word256
  deriving (Eq, Show)

newtype Threshold
  = Threshold
  { unThreshold :: Word32
  } deriving (Eq, Show)

newtype AssetCode4
  = AssetCode4
  { unAssetCode4 :: Word32
  } deriving (Eq, Show)

newtype AssetCode12
  = AssetCode12
  { unAssetCode12 :: Word96
  } deriving (Eq, Show)

newtype DataValue
  = DataValue
  { unDataValue :: ByteString
  } deriving (Eq, Show)

data Asset
  = Native
  | CreditAlphanum4 AssetCode4 PublicKey
  | CreditAlphanum12 AssetCode12 PublicKey
  deriving (Eq, Show)

data Price
  = Price
  { numerator   :: Int32
  , denominator :: Int32
  } deriving (Eq, Show)

newtype Fee
  = Fee
  { unFee :: Word32
  } deriving (Eq, Show)

newtype SequenceNumber
  = SequenceNumber
  { unSequenceNumber :: Int64
  } deriving (Eq, Show)

data TimeBounds
  = TimeBounds
  { timeBoundsMinTime :: Word64
  , timeBoundsMaxTime :: Maybe Word64 -- 0 here means no maxTime
  } deriving (Eq, Show)

newtype Hash
  = Hash
  { unHash :: Word256
  } deriving (Eq, Show)

data Memo
  = MemoNone
  | MemoText Text -- string text<28>;
  | MemoId Word64
  | MemoHash Hash -- the hash of what to pull from the content server
  | MemoReturn Hash -- the hash of the tx you are rejecting
  deriving (Eq, Show)

data AccountOperation
  = AccountOperation
  { accountOperationSourceAccount :: Maybe PublicKey
  , accountOperationOperation     :: Operation
  } deriving (Eq, Show)

data Signer
  = Signer SignerKey Word32
  deriving (Eq, Show)

data CreateAccountOp
  = CreateAccountOp
  { destination     :: PublicKey
  , startingBalance :: Int64
  } deriving (Eq, Show)

data PaymentOp
  = PaymentOp
  { destination :: PublicKey -- recipient of the payment
  , asset       :: Asset     -- what they end up with
  , amount      :: Int64     -- amount they end up with
  } deriving (Eq, Show)

data PathPaymentOp
  = PathPaymentOp
  { sendAsset   :: Asset      -- asset we pay with
  , sendMax     :: Int64      -- the maximum amount of sendAsset to send (excluding fees).
  , destination :: PublicKey  -- recipient of the payment
  , destAsset   :: Asset      -- what they end up with
  , destAmount  :: Int64      -- amount they end up with
  , path        :: [Asset]    -- additional hops it must go through to get there
  } deriving (Eq, Show)

data OfferId
  = NewOffer
  | ExistingOffer Word64
  deriving (Eq, Show)

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
