module Stellar.Parser where

import           Data.Bifunctor              (bimap)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.String                 (String)
import           Data.Tuple.Extra            (thd3)
import           Protolude

type Error = String

parseBinaryB64 :: Binary b => LByteString -> Either Error b
parseBinaryB64 = B64.decode >=> parseBinary

parseBinary :: Binary b => LByteString -> Either Error b
parseBinary = bimap thd3 thd3 . Binary.decodeOrFail
