{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Arrow            (left, (>>>))
import           Control.Lens             ((^.))
import           Control.Newtype          (unpack)
import           Protolude
import           Refined
import           Stellar
import qualified Stellar.Lenses as L
import           Stellar.Printer
import           System.Console.Haskeline
import qualified Data.Text as T

type REPL = InputT IO

main :: IO ()
main = runInputT defaultSettings repl

repl :: REPL ()
repl = do
  putText
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
  putText "(Press Ctrl+D to quit)\n"

  let validKeys :: (KeyPair, KeyPair, KeyPair) -> REPL Bool
      validKeys (srcKeys, dstKeys, issuingKeys)
        | srcKeys == dstKeys
          || srcKeys == issuingKeys
          || issuingKeys == dstKeys =
          putText "Source, Destination and Issuing accounts must be different!\n\
                  \Please enter keys again." >> return False
        | otherwise = return True

  network <- readNetwork

  (sourceKeys, distributionKeys, issuingKeys) <- iterateUntilM validKeys $ do
    sourceKeys <- readKeys "Source Account"
    putText "Keys for the source account are ok"
    distributionKeys <- readKeys "Distribution Account"
    putText "Keys for the distribution account are ok"
    issuingKeys <- readKeys "Issuing Account"
    putText "Keys for the issuing account are ok"
    return (sourceKeys, distributionKeys, issuingKeys)

  srcSequenceNumber <- readSourceAccountSequenceNumber

  code <- readAssetCode

  maxTokens <- readMaxTokens

  putText $ "Asset " <> show (code ^. L.assetCode) <> " defined"

  let sourceAccount = sourceKeys ^. L.publicKey
      issuingAccount = issuingKeys ^. L.publicKey
      distributionAccount = distributionKeys ^. L.publicKey
      asset = AssetCreditAlphanum code issuingAccount
      baseReserve = Stroop 5000000
      baseFee = Stroop 100
      operationsCreateAccounts =
        [ Operation
          { _sourceAccount = Nothing
          , _body = CreateAccount $ CreateAccountOp
            { _destination = distributionAccount
            -- Amount of XLM to send to the newly created account.
            -- This XLM comes from the source account.
            , _startingBalance = 2 * baseReserve
            }
          }
        , Operation
          { _sourceAccount = Nothing
          , _body = CreateAccount $ CreateAccountOp
            { _destination = issuingAccount
            -- Amount of XLM to send to the newly created account.
            -- This XLM comes from the source account.
            , _startingBalance = 3 * baseReserve
            }
          }
        ]

      txCreateAccounts = signTransactionAsEnvelope network sourceKeys $
        Transaction
        { _sourceAccount = sourceAccount
        , _fee = FeeStroops . fromIntegral . unpack
               $ fromIntegral (length operationsCreateAccounts) * baseFee
        , _seqNum = bumpSequenceNumber 1 srcSequenceNumber
        , _timeBounds = Nothing
        , _memo = MemoNone
        , _operations = operationsCreateAccounts
        }

      txCreateTrust = signTransactionAsEnvelope network distributionKeys $
        Transaction
        { _sourceAccount = distributionAccount
        , _fee = FeeStroops . fromIntegral . unpack $ baseFee
        , _seqNum = bumpSequenceNumber 2 srcSequenceNumber
        , _timeBounds = Nothing
        , _memo = MemoNone
        , _operations =
          [ Operation
            { _sourceAccount = Nothing
            , _body = ChangeTrust $ ChangeTrustOp
              { _line = asset
              , _limit = NonNegativeInt64 $$(refineTH maxBound)
              }
            }
          ]
        }

      txAssetCreation = signTransactionAsEnvelope network issuingKeys $
        Transaction
        { _sourceAccount = issuingAccount
        , _fee = FeeStroops . fromIntegral . unpack $ baseFee
        , _seqNum = bumpSequenceNumber 3 srcSequenceNumber
        , _timeBounds = Nothing
        , _memo = MemoNone
        , _operations =
          [ Operation
            { _sourceAccount = Nothing
            , _body = Payment $ PaymentOp
              { _destination = distributionAccount
              , _asset = asset
              , _amount = NonNegativeInt64 $$(refineTH 10)
              }
            }
          ]
        }

  putText $ toS $ printBinaryB64 txCreateAccounts
  putText $ toS $ printBinaryB64 txCreateTrust
  putText $ toS $ printBinaryB64 txAssetCreation


readMaxTokens :: REPL Int64
readMaxTokens = askInput
  "Max amount of tokens (positive integer number)"
  (note "Invalid max amount of tokens" . readMaybe . toS)

readNetwork :: REPL Network
readNetwork = askInput "Stellar network to use ('Testnet' or 'Public')" $
  T.toLower >>> \case
    "public" -> Right Public
    "testnet" -> Right Testnet
    _ -> Left "Invalid network name, must be either 'Testnet' or 'Public'."

readSourceAccountSequenceNumber :: REPL SequenceNumber
readSourceAccountSequenceNumber = askInput
  "Last sequence number of the Source Account"
  (note "Invalid sequence number" . readMaybe . toS)

readAssetCode :: REPL AssetCode
readAssetCode = askInput
  "Asset code (up to 12 characters from the set [a-z][A-Z][0-9])"
  (note "Invalid asset code" . makeAssetCode)

readKeys :: Text -> REPL KeyPair
readKeys tag = flip keyPair <$> readPublicKey tag <*> readSecretKey tag

readSecretKey :: Text -> REPL SecretKey
readSecretKey tag =
  askInput ("[" <> tag <> "] Secret Key") (left show . parseSecretKey)

readPublicKey :: Text -> REPL PublicKey
readPublicKey tag =
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

iterateUntilM :: Monad m => (a -> m Bool) -> m a -> m a
iterateUntilM p ma = ma >>= \a -> ifM (p a) (pure a) (iterateUntilM p ma)
