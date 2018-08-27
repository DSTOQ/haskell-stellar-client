module Data.Binary.Extended
  ( module Data.Binary
  , getEnum
  , putEnum
  ) where

import           Data.Binary
import           Data.Binary.Get (getWord32be)
import           Data.Binary.Put (putWord32be)
import           Protolude       hiding (get, put)

getEnum :: Enum a => Get a
getEnum = getWord32be <&> toEnum . fromIntegral

putEnum :: Enum a => a -> Put
putEnum = putWord32be . fromIntegral . fromEnum
