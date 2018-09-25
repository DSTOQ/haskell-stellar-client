module Control.Monad.Trans.Rest
  ( RestT (..)
  , runRestT
  ) where

import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Network.URI
import           Protolude

import           Control.Monad.Catch            (MonadThrow)
import           Control.Monad.Rest             (ApiBase (..), MonadRest (..),
                                                 RelativeRes (..))
import           Control.Monad.Trans            (MonadTrans)
import           Control.Monad.Trans.Lift.Catch (LiftCatch)
import           Control.Monad.Trans.Lift.StT   (StT)
import           Control.Newtype                (Newtype, pack, unpack)
import qualified Data.Text                      as T


newtype RestT m a
  = RestT (ReaderT ApiBase m a)
  deriving ( Functor
           , Applicative
           , LiftCatch
           , Monad
           , MonadReader ApiBase
           , MonadTrans
           , MonadThrow
           , MonadIO
           )

instance Newtype (RestT m a) (ReaderT ApiBase m a) where
  unpack (RestT r) = r
  pack = RestT

type instance StT RestT a = a

runRestT :: ApiBase -> RestT m a -> m a
runRestT base  = flip runReaderT base . unpack

uriRequest :: ApiBase -> RelativeRes -> Request
uriRequest base res =
  setRequestPath pathBs
  $ setRequestSecure (_scheme base == "https")
  $ setRequestHost hostBs
  defaultRequest
  where
    hostBs = toS $ printUriAuth $ _authority base
    printUriAuth Nothing  = mempty
    printUriAuth (Just a) = mconcat [uriUserInfo a, uriRegName a, uriPort a]
    pathBs = toS $ mconcat
      [ T.intercalate "/" (_path res)
      , T.concat $ "?" : intersperse "&" (kv <$> _query res)
      , maybe "" (mappend "#") (_fragment res)
      ]
    kv :: (Text, Text) -> Text
    kv (k, v) = k <> "=" <> v

instance (MonadThrow m, MonadIO m) => MonadRest (RestT m) where
  get resource = do
    request <- asks (`uriRequest` resource)
    response <- httpJSON $ setRequestMethod methodGet request
    return $ getResponseBody response
