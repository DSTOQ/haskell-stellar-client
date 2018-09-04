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
import           Test.Tasty.Golden          (findByExtension, goldenVsFile)
import           Text.Show.Pretty           (ppShow)

main :: IO ()
main = goldenTests >>= defaultMain

goldenTests :: IO TestTree
goldenTests = do
  b64files <- findByExtension [".txt"] "test/sample"
  pure $ testGroup "Base64 XDR unmarshalling tests"
    [ goldenVsFile
      (takeBaseName b64file)
      goldenFile
      outputFile
      (action b64file outputFile)
    | b64file <- b64files
    , let goldenFile = replaceExtension b64file ".golden"
          outputFile = replaceExtension b64file ".output"
    ]

action :: FilePath -> FilePath -> IO ()
action inputFile outputFile = do
   input <- LBS.readFile inputFile
   printed <- parse input
   LBS.writeFile outputFile $ toS $ ppShow printed

  where

  dropSpaceEnd :: LByteString -> LByteString
  dropSpaceEnd = pack . reverse . dropWhile Char.isSpace . reverse . unpack

  parse :: LByteString -> IO TransactionEnvelope
  parse = either error pure . parseBinaryB64 . dropSpaceEnd
