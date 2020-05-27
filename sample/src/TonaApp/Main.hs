module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Google as TonaGoogle
import Tonatona.Google (Scope(..))
import Tonatona.Google.Client (createDriveFileMultipart, getDriveFileList, postGmailSend)
import Tonatona.Google.Form (Email(..), GetFileParams(..), MultipartBody(..))
import Tonatona.Google.Response (GmailSend(..))
import Tonatona.Google.Type (MediaContent(..), MediaType(..), Metadata(..), QueryString(..), Order(..), SortKey(..))



-- App


app :: RIO Config ()
app = do
  TonaLogger.logInfo $ display ("This is a sample project for tonatona-google-server-api" :: Text)
  -- Choose the function to run depending on the scopes granted to your service key.
  -- postGmailSendSample
  createDriveFileSample

errorHandler :: TonaGoogle.ClientError -> RIO Config ()
errorHandler err =
  TonaLogger.logError $ display $ "Encount error on google-server-api: " <> tshow err

postGmailSendSample :: RIO Config ()
postGmailSendSample =
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

createDriveFileSample :: RIO Config ()
createDriveFileSample =
  TonaGoogle.run errorHandler [ScopeDriveFile, ScopeDriveMetadataRead] $ do
    fileResource <- createDriveFileMultipart $
      MultipartBody
        { metadata = Metadata
            (Just "tonatona-sample")
            (Just $ MediaType "application/vnd.google-apps.document")
            Nothing
        , mediaType = MediaType "text/plain"
        , mediaContent = MediaContent "This is a sample file created by tonatona-google-server-api."
        }
    lift $ TonaLogger.logDebug $ display ("The file has been successfully created: " <> tshow fileResource)

    list <- getDriveFileList $
      GetFileParams
        { query = Just $ QueryString "modifiedTime > '2012-06-04T12:00:00'"
        , orderBy = Just [ Desc ModifiedTime
                         , Asc Name
                         ]
        }
    lift $ TonaLogger.logDebug $ display ("File list: " <> tshow list)



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
