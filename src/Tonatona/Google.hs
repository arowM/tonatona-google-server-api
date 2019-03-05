{-# LANGUAGE CPP #-}
module Tonatona.Google
  ( run
  , Dsl
  , DslBackend
  , Config
  , ClientError
  , JWT
  , JWT.Scope(..)
  ) where

import Tonalude

import qualified Google.Client as Client
import Google.JWT (JWT)
import qualified Google.JWT as JWT
#if MIN_VERSION_servant_client(0, 16, 0)
import Servant.Client (ClientError)
#else
import Servant.Client (ServantError)
#endif

import Tonatona (HasConfig(..), HasParser(..))
import TonaParser (Parser, (.||), argLong, envVar, requiredVal)
import Tonatona.Google.Internal

#if !MIN_VERSION_servant_client(0, 16, 0)
type ClientError = ServantError
#endif

-- | Main function.
run ::
     (HasConfig env Config)
  => (ClientError -> RIO env a)
  -- ^ Error handler
  -> [JWT.Scope]
  -> Dsl env a
  -> RIO env a
run errorHandler scopes query = do
  Config {..} <- asks config
  eToken <- liftIO $ Client.getToken (Just delegationEmail) jwt scopes
  case eToken of
    Left err -> errorHandler err
    Right token ->
      runReaderT query (DslBackend token) `catch` errorHandler


------------
-- Config --
------------

data Config = Config
  { serviceKeyFile :: ServiceKeyFile
  , jwt :: JWT
  , delegationEmail :: JWT.Email
  }

newtype ServiceKeyFile = ServiceKeyFile { unServiceKeyFile :: FilePath }
  deriving (Eq, Read, Show)

instance HasParser ServiceKeyFile where
  parser = ServiceKeyFile <$>
    requiredVal
      "File path to service key file (Example: \"gmail-3abc452de.json\")"
      ( argLong "service-key-file" .|| envVar "SERVICE_KEY_FILE")

parseJwt :: ServiceKeyFile -> Parser JWT
parseJwt (ServiceKeyFile fp) = do
  mjwt <- liftIO $ JWT.readServiceKeyFile fp
  case mjwt of
    Nothing ->
      error $ "Failed to parse service key file: " <> fp
    Just jwt ->
      pure jwt

parseEmail :: Parser JWT.Email
parseEmail = JWT.Email <$>
  requiredVal
    "Email account to use for delegation."
    ( argLong "delegation-email" .|| envVar "DELEGATION_EMAIL")

instance HasParser Config where
  parser = do
    keyFile <- parser
    Config
      <$> pure keyFile
      <*> parseJwt keyFile
      <*> parseEmail
