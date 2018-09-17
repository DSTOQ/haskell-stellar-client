module Stellar.Monad
  ( StellarM (..)
  ) where

import           Protolude
import           Stellar.Client.Types

class Monad m => StellarM m where
  account :: AccountId -> m Account
