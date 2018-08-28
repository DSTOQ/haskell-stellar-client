{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Binary.Extended
  ( module Data.Binary
  , getEnum
  , putEnum
  , Padded (..)
  , Limited (..)
  ) where

import           Control.Monad   (fail)
import           Data.Binary
import           Data.Binary.Get (getByteString, getWord32be, skip)
import           Data.Binary.Put (putByteString, putWord32be)
import           Data.ByteString as BS
import           GHC.TypeLits
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

newtype Limited (n :: Nat) a
  = Limited
  { unLimited :: a
  } deriving (Eq, Show, Functor)

instance KnownNat n => Binary (Limited n ByteString) where
  put (Limited bs) = putWord32be len >> putByteString padded
    where
    len = fromIntegral l
    padded = bs <> BS.replicate (padding 4 l) 0
    l = BS.length bs
  get = do
    len <- getWord32be <&> fromIntegral
    let cap = fromInteger $ natVal (Proxy :: Proxy n)
    if len > cap
      then fail $ "Max length (" <> show cap <> ") exceeded (" <> show len <> ")"
      else do
        bs <- getByteString len
        skip $ padding 4 len
        pure $ Limited bs

instance KnownNat n => Binary (Limited n Text) where
  put = put . fmap (toS :: Text -> ByteString)
  get = (fmap . fmap) (toS :: ByteString -> Text) get


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
