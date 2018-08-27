{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import           Data.LargeWord       (Word256)
import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import           Protolude
import           Stellar.Types

main :: IO Bool
main = checkParallel $$(discover)

roundtrip :: (Binary a, Eq a, Show a) => Gen a -> Property
roundtrip gen = property $ do
  v <- forAll gen
  decode (encode v) === v

isMultiple4 :: (Binary a, Show a) => Gen a -> Property
isMultiple4 gen = property $ do
  v <- forAll gen
  BL.length (encode v) `rem` 4 === 0


genCryptoKeyType :: Gen CryptoKeyType
genCryptoKeyType = Gen.enumBounded

prop_roundtrip_crypto_key_type :: Property
prop_roundtrip_crypto_key_type = roundtrip genCryptoKeyType

prop_multiple4_crypto_key_type :: Property
prop_multiple4_crypto_key_type = isMultiple4 genCryptoKeyType


genPublicKeyType :: Gen PublicKeyType
genPublicKeyType = Gen.enumBounded

prop_roundtrip_pub_key_type :: Property
prop_roundtrip_pub_key_type = roundtrip genPublicKeyType

prop_multiple4_pub_key_type :: Property
prop_multiple4_pub_key_type = isMultiple4 genPublicKeyType


genPublicKey :: Gen PublicKey
genPublicKey = PublicKeyEd25519 <$> Gen.integral range
  where range :: Range Word256
        range = Range.exponentialBounded

prop_roundtrip_pub_key :: Property
prop_roundtrip_pub_key = roundtrip genPublicKey

prop_multiple4_pub_key :: Property
prop_multiple4_pub_key = isMultiple4 genPublicKey
