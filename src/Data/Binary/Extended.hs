{-# LANGUAGE ScopedTypeVariables #-}

module Data.Binary.Extended
  ( module Data.Binary
  , getEnum
  , putEnum
  , getZeroPaddedByteString
  , putZeroPaddedByteString
  , Padded (..)
  ) where

import           Data.Binary
import           Data.Binary.Get (getByteString, getWord32be, skip)
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
  m = p * if r == 0 then q else (q + 1)
  (q, r) = l `quotRem` p

newtype Padded a
  = Padded
  { unPadded :: a
  } deriving (Eq, Show, Functor)

instance Binary (Padded ByteString) where
  get = Padded <$> getZeroPaddedByteString
  put = putZeroPaddedByteString . unPadded

getZeroPaddedByteString :: Get ByteString
getZeroPaddedByteString = do
  len <- getWord32be <&> fromIntegral
  bs <- getByteString len
  skip $ padding 4 len
  pure bs

putZeroPaddedByteString :: ByteString -> Put
putZeroPaddedByteString bs = putWord32be len >> putByteString padded
  where
  len = fromIntegral l
  padded = s <> BS.replicate (padding 4 l) 0
  l = BS.length s
  s = BS.take 64 bs

instance Binary (Padded Text) where
  get = fmap toS <$> (get :: Get (Padded ByteString))
  put = put . fmap (toS :: Text -> ByteString)

instance Binary a => Binary (Padded (Maybe a)) where
  put (Padded Nothing)  = putWord32be 0
  put (Padded (Just a)) = putWord32be 1 >> put a
  get = do
    w <- getWord32be
    case w of
      0 -> pure $ Padded Nothing
      _ -> liftM (Padded . Just) get
