module Control.Monad.Rest
  ( MonadRest (..)
  , ApiBase (..)
  , apiBase
  , RelativeRes (..)
  , relativeRes
  ) where

import Named
import Network.URI
import Protolude           hiding (get)

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans (MonadTrans)
import Data.Aeson.Types    hiding (String)


data ApiBase
  = ApiBase
  { _scheme    :: Text
  , _authority :: Maybe URIAuth
  } deriving (Eq, Show)

data RelativeRes
  = RelativeRes
  { _path     :: [Text]
  , _query    :: [(Text, Text)]
  , _fragment :: Maybe Text
  } deriving (Eq, Show)

apiBase :: URI -> ApiBase
apiBase uri = ApiBase (toS $ uriScheme uri) (uriAuthority uri)

relativeRes
  :: "path" :? [Text]
  -> "query" :? [(Text, Text)]
  -> "fragment" :? Maybe Text
  -> RelativeRes
relativeRes
  (argDef #path [] -> p)
  (argDef #query [] -> q)
  (argDef #fragment Nothing -> f)
  = RelativeRes p q f

class MonadThrow m => MonadRest m where
  get :: FromJSON v => RelativeRes -> m v

  default get :: ( FromJSON v
                 , MonadTrans t
                 , MonadRest m1
                 , m ~ t m1
                 ) => RelativeRes -> m v
  get = lift . get

instance MonadRest m => MonadRest (ReaderT r m)
instance MonadRest m => MonadRest (ExceptT e m)
