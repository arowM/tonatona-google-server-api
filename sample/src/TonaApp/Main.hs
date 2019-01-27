module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Google as TonaGoogle
import Tonatona.Google (Scope(..))
import Tonatona.Google.Client (postGmailSend)
import Tonatona.Google.Form (Email(..))
import Tonatona.Google.Response (GmailSend(..))



-- App


app :: RIO Config ()
app = do
  TonaLogger.logInfo $ display ("This is a sample project for tonatona-google-server-api" :: Text)
  TonaGoogle.run errorHandler [ScopeGmailSend] $ do
    GmailSend sentId <- postGmailSend $ Email
      { to = "to@example.com"
      , from = "from@example.com"
      , replyTo = Nothing
      , ccs = []
      , subject = "Test email"
      , body = "This is a test email. Sent by tonatona-google-server-api"
      }
    lift $ TonaLogger.logDebug $ display ("Successfully sent by gmail: " <> sentId)


errorHandler :: TonaGoogle.ServantError -> RIO Config ()
errorHandler err =
  TonaLogger.logError $ display $ "Encount error on google-server-api: " <> tshow err



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaGoogle :: TonaGoogle.Config
  }


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger


instance HasConfig Config TonaGoogle.Config where
  config = tonaGoogle


instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
