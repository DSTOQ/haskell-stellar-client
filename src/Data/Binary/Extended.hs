{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Binary.Extended
  ( module Data.Binary
  , getEnum
  , putEnum
  , Padded (..)
  , putPaddedByteString
  , padding
  ) where

import           Data.Binary
import           Data.Binary.Get (getWord32be)
import           Data.Binary.Put (putByteString, putWord32be)
import           Data.ByteString as BS
import           Protolude       hiding (get, put, putByteString)

getEnum :: Enum a => Get a
getEnum = getWord32be <&> toEnum . fromIntegral

putEnum :: Enum a => a -> Put
putEnum = putWord32be . fromIntegral . fromEnum

padding :: Int -> Int -> Int
padding p l = m - l
  where
  m = p * if r == 0 then q else q + 1
  (q, r) = l `quotRem` p

putPaddedByteString :: ByteString -> Put
putPaddedByteString bs = do
  let len = BS.length bs
      pad = padding 4 len
  putWord32be $ fromIntegral len
  putByteString $ bs <> BS.replicate pad 0

newtype Padded a
  = Padded
  { unPadded :: a
  } deriving (Eq, Show, Functor)

instance Binary (Padded Bool) where
  get = getWord32be <&> (> 0) <&> Padded
  put (Padded True)  = putWord32be 1
  put (Padded False) = putWord32be 0


instance Binary a => Binary (Padded (Maybe a)) where
  put (Padded Nothing)  = putWord32be 0
  put (Padded (Just a)) = putWord32be 1 >> put a
  get = do
    w <- getWord32be
    case w of
      0 -> pure $ Padded Nothing
      _ -> Padded . Just <$> get
