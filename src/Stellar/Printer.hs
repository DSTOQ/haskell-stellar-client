module Stellar.Printer where

import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BL
import           Protolude

printBinaryB64 :: Binary b => b -> ByteString
printBinaryB64 = B64.encode . BL.toStrict . Binary.encode
