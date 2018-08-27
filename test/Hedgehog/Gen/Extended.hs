module Hedgehog.Gen.Extended
  ( module Hedgehog.Gen
  , word96
  , word256
  , expWord32
  , expWord64
  , expInt32
  , expInt64
  ) where

import           Data.Int
import           Data.LargeWord (Word256, Word96)
import           Data.Word
import           Hedgehog       (Gen, Range)
import           Hedgehog.Gen
import qualified Hedgehog.Range as Range

word96 :: Range Word96 -> Gen Word96
word96 = integral

word256 :: Range Word256 -> Gen Word256
word256 = integral

expWord32 :: Gen Word32
expWord32 = word32 Range.exponentialBounded

expWord64 :: Gen Word64
expWord64 = word64 Range.exponentialBounded

expInt32 :: Gen Int32
expInt32 = int32 Range.exponentialBounded

expInt64 :: Gen Int64
expInt64 = int64 Range.exponentialBounded
