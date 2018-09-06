module Main where

import           Control.Arrow            (left)
import           Control.Lens             ((^.))
import           Control.Monad.Loops      (iterateUntil)
import           Protolude
import           Stellar
import           Stellar.Printer
import           System.Console.Haskeline

type REPL = InputT IO

main :: IO ()
main = runInputT defaultSettings repl

repl :: REPL ()
repl = do
  outputStrLn
    "------------------------------------------------------------------------- \n\
    \In order to distribute a custom asset or token on the Stellar Network,    \n\
    \three unique accounts will be used. First, is the source account.         \n\
    \The source account is the account of the entity looking to create         \n\
    \a new token. Second is the issuing account. The issuing account is        \n\
    \created by the source account as a mechanism to create new tokens.        \n\
    \The third account is the distribution account. The goal of distribution   \n\
    \account is to act as the mechanism for distributing tokens to the public. \n\
    \------------------------------------------------------------------------- \n\
    \https://www.stellar.org/developers/guides/walkthroughs/custom-assets.html \n\
    \------------------------------------------------------------------------- \n"
  outputStrLn "(Press Ctrl+D to quit)\n"

  (sourceKeys, distributionKeys) <- iterateUntil (uncurry (/=)) $ do
    sourceKeys <- readKeys "Source Account"
    putText "Keys for the source account are ok"
    distributionKeys <- readKeys "Distribution Account"
    putText "Keys for the distribution account are ok"
    return (sourceKeys, distributionKeys)

  code <- readAssetCode
  let _ = AssetCreditAlphanum code (sourceKeys ^. publicKey)
  putText $ "Asset " <> show (code ^. assetCode) <> " defined"

  let tx1 =
        Transaction
        { _sourceAccount = sourceKeys ^. publicKey
        , _fee = Fee 10 -- TODO: starting balance + tx fee ?
        , _seqNum = SequenceNumber 1
        , _timeBounds = Nothing
        , _memo = MemoNone
        , _operations =
          [ Operation
            { _sourceAccount = Nothing
            , _body = CreateAccount $ CreateAccountOp
              { _destination = distributionKeys ^. publicKey

              -- Amount of XLM to send to the newly created account.
              -- This XLM comes from the source account.
              , _startingBalance = 10 -- TODO: proper balance
              }
            }
          ]
        }
  putText $ toS $ printBinaryB64 tx1


readAssetCode :: REPL AssetCode
readAssetCode = askInput
  "Asset code (up to 12 characters from the set [a-z][A-Z][0-9])"
  (note "Invalid asset code" . makeAssetCode)

readKeys :: Text -> REPL KeyPair
readKeys tag = keyPair <$> readSecretKey tag <*> readPublicKey tag

readSecretKey :: Text -> REPL SecretKey
readSecretKey tag = -- SC6ZH63HM2AB5M2RIUNMNRKM26KQTKWIKH4MY5PMTXBZPVOHCXB5FUNX
  askInput ("[" <> tag <> "] Secret Key") (left show . parseSecretKey)

readPublicKey :: Text -> REPL PublicKey
readPublicKey tag = -- GCVXHAFVVRGBKHKPODVHBE4UENYCEARM2IK355YDNX5ZMXX3V7YIWBBI
  askInput ("[" <> tag <> "] Public Key") (left show . parsePublicKey)

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
