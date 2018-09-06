module Main where

import           Control.Arrow            (left)
import           Control.Lens             ((^.))
import           Protolude
import           Stellar
import           System.Console.Haskeline

type REPL = InputT IO

main :: IO ()
main = runInputT defaultSettings repl

repl :: REPL ()
repl = do
  outputStrLn "Let's prepare transactions to issue a custom asset!"
  outputStrLn "(Press Ctrl+D to quit)"
  keys <- readKeys
  putText "Keypair parsed"
  code <- readAssetCode
  let _ = AssetCreditAlphanum code (keys ^. publicKey)
  putText $ "Asset " <> show (code ^. assetCode) <> " defined"


readAssetCode :: REPL AssetCode
readAssetCode = askInput
  "Asset code (up to 12 characters from the set [a-z][A-Z][0-9])"
  (note "Invalid asset code" . makeAssetCode)


readKeys :: REPL KeyPair
readKeys = do
  -- SC6ZH63HM2AB5M2RIUNMNRKM26KQTKWIKH4MY5PMTXBZPVOHCXB5FUNX
  secret <- askInput "Secret Key" (left show . parseSecretKey)
  -- GCVXHAFVVRGBKHKPODVHBE4UENYCEARM2IK355YDNX5ZMXX3V7YIWBBI
  public <- askInput "Public Key" (left show . parsePublicKey)
  return $ keyPair secret public

askInput :: Text -> (Text -> Either Text a) -> REPL a
askInput q p = do
  answer <- getInputLine $ toS $ q <> "? "
  case answer of
    Nothing -> outputStrLn "Bye!" >> liftIO exitSuccess
    Just a  -> either again return $ p $ toS a
  where
  again err = do
    outputStrLn $ toS err
    askInput q p
