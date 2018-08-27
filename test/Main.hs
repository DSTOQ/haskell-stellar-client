{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}

module Main where

import           Data.Binary
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
  = Checkable (forall a. (Binary a, Show a, Eq a) => Gen a -> Property)

data Genable
  = forall a . (Binary a, Show a, Eq a)
  => Genable (String, Gen a)

runProperties :: IO Bool
runProperties = checkParallel $ Group "Binary Properties" $
  mkProp <$> checks <*> gens
  where

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
    [ Genable ("PublicKeyType",  genPublicKeyType)
    , Genable ("PublicKey",      genPublicKey)
    , Genable ("SignerKeyType",  genSignerKeyType)
    , Genable ("SignerKey",      genSignerKey)
    , Genable ("Threshold",      genThreshold)
    , Genable ("AssetType",      genAssetType)
    , Genable ("AssetCode4",     genAssetCode4)
    , Genable ("AssetCode12",    genAssetCode12)
    , Genable ("DataValue",      genDataValue)
    , Genable ("Asset",          genAsset)
    , Genable ("Price",          genPrice)
    , Genable ("Fee",            genFee)
    , Genable ("SequenceNumber", genSequenceNumber)
    , Genable ("TimeBounds",     genTimeBounds)
    , Genable ("Hash",           genHash)
    , Genable ("Memo",           genMemo)
    , Genable ("Signer",         genSigner)
    , Genable ("PaymentOp",      genPaymentOp)
    , Genable ("PathPaymentOp",  genPathPaymentOp)
    , Genable ("OfferId",        genOfferId)
    ]

roundtrip :: Checkable
roundtrip = Checkable $ \gen -> property $ do
  v <- forAll gen
  case decodeOrFail (encode v) of
    Left _ -> failure
    Right (unconsumed, _, decoded) -> do
      decoded === v
      assert $ BL.null unconsumed

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

genDataValue :: Gen DataValue
genDataValue = DataValue <$> Gen.bytes (Range.linear 0 64)

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
  , MemoText <$> Gen.text (Range.linear 0 27) Gen.ascii
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
