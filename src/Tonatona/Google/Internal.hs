module Tonatona.Google.Internal
  ( Dsl
  , DslBackend(..)
  ) where

import Tonalude
import Google.Response (Token)

type Dsl env
  = ReaderT DslBackend (RIO env)

data DslBackend = DslBackend
  { token :: Token
  }
  deriving (Show, Eq)
