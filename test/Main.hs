{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Main where

import           Data.Binary.Extended
import qualified Data.ByteString.Lazy  as BL
import           Data.String           (String, fromString)
import           Hedgehog
import qualified Hedgehog.Gen.Extended as Gen
import qualified Hedgehog.Range        as Range
import           Protolude
import           Stellar.Types
import           System.Exit           (exitFailure)

main :: IO ()
main = ifM runProperties exitSuccess exitFailure

newtype Checkable
  = Checkable (∀ a. (Binary a, Show a, Eq a) => Gen a -> Property)

data Genable
  = ∀ a . (Binary a, Show a, Eq a)
  => Genable (String, Gen a)

runProperties :: IO Bool
runProperties = checkParallel $ Group "Binary Properties" $
  mkProp <$> checks <*> gens

mkProp :: (String, Checkable) -> Genable -> (PropertyName, Property)
mkProp (checkName, Checkable f) (Genable (typeName, g)) =
   (fromString $ checkName <> " " <> typeName, f g)

checks :: [(String, Checkable)]
checks =
  [ ("Roundtrip",     roundtrip)
  , ("Multiple of 4", isMultiple4)
  ]

gens :: [Genable]
gens =
  [ Genable ("PublicKeyType",        genPublicKeyType)
  , Genable ("PublicKey",            genPublicKey)
  , Genable ("SignerKeyType",        genSignerKeyType)
  , Genable ("SignerKey",            genSignerKey)
  , Genable ("Threshold",            genThreshold)
  , Genable ("AssetType",            genAssetType)
  , Genable ("AssetCode4",           genAssetCode4)
  , Genable ("AssetCode12",          genAssetCode12)
  , Genable ("Asset",                genAsset)
  , Genable ("Price",                genPrice)
  , Genable ("Fee",                  genFee)
  , Genable ("SequenceNumber",       genSequenceNumber)
  , Genable ("TimeBounds",           genTimeBounds)
  , Genable ("Hash",                 genHash)
  , Genable ("Memo",                 genMemo)
  , Genable ("Signer",               genSigner)
  , Genable ("PaymentOp",            genPaymentOp)
  , Genable ("PathPaymentOp",        genPathPaymentOp)
  , Genable ("OfferId",              genOfferId)
  , Genable ("ManageOfferOp",        genManageOfferOp)
  , Genable ("CreatePassiveOfferOp", genCreatePassiveOfferOp)
  , Genable ("HomeDomain",           genHomeDomain)
  , Genable ("SetOptionsOp",         genSetOptionsOp)
  , Genable ("ChangeTrustOp",        genChangeTrustOp)
  , Genable ("AllowTrustOp",         genAllowTrustOp)
  , Genable ("DataValue",            genDataValue)
  , Genable ("ManageDataOp",         genManageDataOp)
  , Genable ("OperationType",        genOperationType)
  , Genable ("Operation",            genOperation)
  , Genable ("AccountOperation",     genAccountOperation)
  , Genable ("Transaction",          genTransaction)
  , Genable ("Signature",            genSignature)
  , Genable ("SignatureHint",        genSignatureHint)
  , Genable ("DecoratedSignature",   genDecoratedSignature)
  , Genable ("TransactionEnvelope",  genTransactionEnvelope)
  ]

roundtrip :: Checkable
roundtrip = Checkable $ \gen -> property $ do
  v <- forAll gen
  let encoded = encode v
  annotateShow encoded
  case decodeOrFail encoded of
    Left (_, _, err) -> do
      annotate err
      failure
    Right (unconsumed, _, decoded) -> do
      decoded === v
      BL.length unconsumed === 0

isMultiple4 :: Checkable
isMultiple4 = Checkable $ \gen -> property $ do
  v <- forAll gen
  BL.length (encode v) `rem` 4 === 0



genPublicKeyType :: Gen PublicKeyType
genPublicKeyType = Gen.enumBounded

genPublicKey :: Gen PublicKey
genPublicKey = PublicKeyEd25519 <$> Gen.word256 Range.exponentialBounded

genSignerKeyType :: Gen SignerKeyType
genSignerKeyType = Gen.enumBounded

genSignerKey :: Gen SignerKey
genSignerKey = Gen.element constructors <*> Gen.word256 Range.exponentialBounded
  where constructors = [SignerKeyEd25519, SignerKeyPreAuthTx, SignerKeyHashX]

genThreshold :: Gen Threshold
genThreshold = Threshold <$> Gen.expWord32

genAssetCode4 :: Gen AssetCode4
genAssetCode4 = AssetCode4 <$> Gen.expWord32

genAssetCode12 :: Gen AssetCode12
genAssetCode12 = AssetCode12 <$> Gen.word96 Range.exponentialBounded

genAssetType :: Gen AssetType
genAssetType = Gen.enumBounded

genAsset :: Gen Asset
genAsset = Gen.choice
  [ pure AssetNative
  , AssetCreditAlphanum4 <$> genAssetCode4 <*> genPublicKey
  , AssetCreditAlphanum12 <$> genAssetCode12 <*> genPublicKey
  ]

genPrice :: Gen Price
genPrice = Price <$> Gen.expInt32 <*> Gen.expInt32

genFee :: Gen Fee
genFee = Fee <$> Gen.expWord32

genSequenceNumber :: Gen SequenceNumber
genSequenceNumber = SequenceNumber <$> Gen.expInt64

genTimeBounds :: Gen TimeBounds
genTimeBounds = TimeBounds <$> Gen.expWord64 <*> Gen.maybe Gen.expWord64

genHash :: Gen Hash
genHash = Hash <$> Gen.word256 Range.exponentialBounded

genMemo :: Gen Memo
genMemo = Gen.choice
  [ pure MemoNone
  , MemoText . VarLen <$> Gen.bytes (Range.linear 0 27)
  , MemoId <$> Gen.expWord64
  , MemoHash <$> genHash
  , MemoReturn <$> genHash
  ]

genSigner :: Gen Signer
genSigner = Signer
  <$> genSignerKey
  <*> Gen.expWord32

genCreateAccountOp :: Gen CreateAccountOp
genCreateAccountOp = CreateAccountOp
  <$> genPublicKey
  <*> Gen.expInt64

genPaymentOp :: Gen PaymentOp
genPaymentOp = PaymentOp
  <$> genPublicKey
  <*> genAsset
  <*> Gen.expInt64

genPathPaymentOp :: Gen PathPaymentOp
genPathPaymentOp = PathPaymentOp
  <$> genAsset
  <*> Gen.expInt64
  <*> genPublicKey
  <*> genAsset
  <*> Gen.expInt64
  <*> Gen.list (Range.linear 0 5) genAsset

genOfferId :: Gen OfferId
genOfferId = OfferId <$> Gen.expWord64

genManageOfferOp :: Gen ManageOfferOp
genManageOfferOp = ManageOfferOp
  <$> genAsset
  <*> genAsset
  <*> Gen.expInt64
  <*> genPrice
  <*> genOfferId

genCreatePassiveOfferOp :: Gen CreatePassiveOfferOp
genCreatePassiveOfferOp = CreatePassiveOfferOp
  <$> genAsset
  <*> genAsset
  <*> Gen.expInt64
  <*> genPrice

genHomeDomain :: Gen HomeDomain
genHomeDomain = HomeDomain . VarLen <$> Gen.text (Range.linear 1 32) Gen.ascii

genSetOptionsOp :: Gen SetOptionsOp
genSetOptionsOp = SetOptionsOp
  <$> Gen.maybe genPublicKey
  <*> Gen.maybe Gen.expWord32
  <*> Gen.maybe Gen.expWord32
  <*> Gen.maybe Gen.expWord32
  <*> Gen.maybe genThreshold
  <*> Gen.maybe genThreshold
  <*> Gen.maybe genThreshold
  <*> Gen.maybe genHomeDomain
  <*> Gen.maybe genSigner

genChangeTrustOp :: Gen ChangeTrustOp
genChangeTrustOp = ChangeTrustOp
  <$> genAsset
  <*> Gen.maybe Gen.expInt64

genAllowTrustOp :: Gen AllowTrustOp
genAllowTrustOp = AllowTrustOp
  <$> genPublicKey
  <*> Gen.either genAssetCode4 genAssetCode12
  <*> Gen.bool

genDataValue :: Gen DataValue
genDataValue = DataValue . VarLen <$> Gen.bytes (Range.linear 0 64)

genManageDataOp :: Gen ManageDataOp
genManageDataOp = ManageDataOp
  <$> (VarLen <$> Gen.text (Range.linear 1 10) Gen.ascii)
  <*> Gen.maybe genDataValue

genOperationType :: Gen OperationType
genOperationType = Gen.element
  [ OperationTypeCreateAccount
  , OperationTypePayment
  , OperationTypePathPayment
  , OperationTypeManageOffer
  , OperationTypeCreatePassiveOffer
  , OperationTypeSetOptions
  , OperationTypeChangeTrust
  , OperationTypeAllowTrust
  , OperationTypeAccountMerge
  , OperationTypeInflation
  , OperationTypeManageData
  , OperationTypeBumpSequence
  ]

genOperation :: Gen Operation
genOperation = Gen.choice
  [ CreateAccount <$> genCreateAccountOp
  , Payment <$> genPaymentOp
  , PathPayment <$> genPathPaymentOp
  , ManageOffer <$> genManageOfferOp
  , CreatePassiveOffer <$> genCreatePassiveOfferOp
  , SetOptions <$> genSetOptionsOp
  , ChangeTrust <$> genChangeTrustOp
  , AllowTrust <$> genAllowTrustOp
  , AccountMerge <$> genPublicKey
  , pure Inflation
  , ManageData <$> genManageDataOp
  , BumpSequence <$> genSequenceNumber
  ]

genAccountOperation :: Gen AccountOperation
genAccountOperation = AccountOperation
  <$> Gen.maybe genPublicKey
  <*> genOperation

genTransaction :: Gen Transaction
genTransaction = Transaction
  <$> genPublicKey
  <*> genFee
  <*> genSequenceNumber
  <*> Gen.maybe genTimeBounds
  <*> genMemo
  <*> Gen.nonEmpty (Range.exponential 1 10) genAccountOperation

genSignatureHint :: Gen SignatureHint
genSignatureHint = SignatureHint <$> Gen.expWord32

genSignature :: Gen Signature
genSignature = Signature . FixLen <$> Gen.bytes (Range.singleton 256)

genDecoratedSignature :: Gen DecoratedSignature
genDecoratedSignature = DecoratedSignature
  <$> genSignatureHint
  <*> genSignature

genTransactionEnvelope :: Gen TransactionEnvelope
genTransactionEnvelope = TransactionEnvelope
  <$> genTransaction
  <*> Gen.list (Range.linear 0 3) genDecoratedSignature
