module Control.Monad.Trans.Rest
  ( RestT (..)
  , runRestT
  ) where

import           Control.Lens
import           Control.Monad.Catch            (MonadThrow, throwM)
import           Control.Monad.Rest             (ApiBase, MonadRest (..),
                                                 resourceUri)
import           Control.Monad.Trans            (MonadTrans)
import           Control.Monad.Trans.Lift.Catch (LiftCatch)
import           Control.Monad.Trans.Lift.StT   (StT)
import           Control.Newtype                (Newtype, pack, unpack)
import           Data.Aeson                     (Result (..), fromJSON)
import           Network.HTTP.Client            (HttpException (InvalidUrlException))
import           Network.HTTP.Req
import           Protolude
import           Text.URI
import qualified Text.URI.Lens                  as L

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

instance MonadIO m => MonadHttp (RestT m) where
  handleHttpException = throwIO

instance (MonadThrow m, MonadIO m) => MonadRest (RestT m) where
  get resource = do
    uri <- asks (`resourceUri` resource)
    response <- withUri uri $ \u -> req GET u NoReqBody jsonResponse mempty
    case fromJSON (responseBody response) of
      Error s   -> throwM (JsonHttpException s)
      Success a -> return a

withUri :: MonadThrow m => URI -> (forall s . Url s -> m r) -> m r
withUri uri f = fromMaybe err $ urlHttps <|> urlHttp
  where
  err = throwM $ InvalidUrlException (renderStr uri) "Invalid URL scheme"
  urlHttp = fmap f (toUrlHttp uri >>= (<$> host))
  urlHttps = fmap f (toUrlHttps uri >>= (<$> host))
  host = unRText <$> uri ^? (L.uriAuthority . _Right  . L.authHost)

toUrlHttp :: URI -> Maybe (Text -> Url 'Http)
toUrlHttp uri = uriScheme uri <&> unRText & \case
  Just "http" -> Just http
  _ -> Nothing

toUrlHttps :: URI -> Maybe (Text -> Url 'Https)
toUrlHttps uri = uriScheme uri <&> unRText & \case
  Just "https" -> Just https
  _ -> Nothing
