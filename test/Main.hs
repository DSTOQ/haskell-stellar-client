module Main where

import qualified Data.ByteString.Lazy       as LBS
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.Char                  as Char
import           Prelude                    (error)
import           Protolude
import           Stellar
import           Stellar.Parser
import           System.FilePath            (replaceExtension, takeBaseName)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden          (findByExtension, goldenVsString)
import           Text.Show.Pretty           (ppShow)

main :: IO ()
main = goldenTests >>= defaultMain

goldenTests :: IO TestTree
goldenTests = do
  b64files <- findByExtension [".txt"] "test/sample"
  pure $ testGroup "Base64 XDR unmarshalling tests"
    [ goldenVsString (takeBaseName b64file) goldenFile (action b64file)
    | b64file <- b64files
    , let goldenFile = replaceExtension b64file ".golden"
    ]

action :: FilePath -> IO LByteString
action = LBS.readFile >=> parse >=> printB64
  where

  dropSpaceEnd :: LByteString -> LByteString
  dropSpaceEnd = pack . reverse . dropWhile Char.isSpace . reverse . unpack

  parse :: LByteString -> IO TransactionEnvelope
  parse = either error pure . parseBinaryB64 . dropSpaceEnd

  printB64 :: TransactionEnvelope -> IO LByteString
  printB64 = pure . toS . ppShow
