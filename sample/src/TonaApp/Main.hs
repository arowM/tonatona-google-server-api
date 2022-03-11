module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Google as TonaGoogle
import Tonatona.Google (Scope(..))
import Tonatona.Google.Client (createDriveFileMultipart, getDriveFileList, postGmailSend, getCalendarEventList, postCalendarEvent)
import Tonatona.Google.Form (Account(..), DateTime(..), Email(..), GetFileParams(..), MultipartBody(..), CalendarEvent(..), ExtendedProperty(..), ExtendedProperties(..))
import Tonatona.Google.Response (GmailSend(..))
import Tonatona.Google.Type (MediaContent(..), MediaType(..), Metadata(..), QueryString(..), Order(..), SortKey(..))
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.Calendar (addDays)
import qualified TonaApp.JST as JST
import qualified Data.HashMap.Strict as HashMap


-- App


app :: RIO Config ()
app = do
  TonaLogger.logInfo $ display ("This is a sample project for tonatona-google-server-api" :: Text)
  -- Choose the function to run depending on the scopes granted to your service key.
  -- postGmailSendSample
  -- createDriveFileSample
  -- createCalendarEventSample
  getCalendarSample

errorHandler :: TonaGoogle.ClientError -> RIO Config ()
errorHandler err =
  TonaLogger.logError $ display $ "Encount error on google-server-api: " <> tshow err

createCalendarEventSample :: RIO Config ()
createCalendarEventSample =
  TonaGoogle.run errorHandler [ScopeCalendarFull] $ do
    today <- liftIO JST.getCurrentDay
    let
      gmail = "example@gmail.com"
      start = DateTime . zonedTimeToUTC $ JST.zonedTime today 9 0 0
      end = DateTime . zonedTimeToUTC $ JST.zonedTime today 10 0 0
      privateExtendedProperties =
        HashMap.fromList
          [ ("pr", "baz")
          ]
      sharedExtendedProperties =
        HashMap.fromList
          [ ("sh", "bar")
          ]
      extendedProperties = ExtendedProperties
        { private = privateExtendedProperties
        , shared = sharedExtendedProperties
        }
      form =
        CalendarEvent
          { creator = Account gmail
          , attendees = [Account gmail]
          , summary = "tonatona-google-server-api postCalenarEvent sample"
          , description = ""
          , start = start
          , end = end
          , extendedProperties = Just extendedProperties
          }
    calendarEvent <- postCalendarEvent form
    lift $ TonaLogger.logDebug $ display $ tshow calendarEvent

getCalendarSample :: RIO Config ()
getCalendarSample =
  TonaGoogle.run errorHandler [ScopeCalendarFull] $ do
    today <- liftIO JST.getCurrentDay
    let
      calendarId = "example@gmail.com"
      since = 0
      margin = 7
      margin' = 2 * margin + 2 -- `margin` + (土日などの除外する日数) を超える日数
      singleEvents = pure True
      orderBy = pure "startTime"
      timeMin = Just . DateTime . zonedTimeToUTC . JST.midnight $ addDays since today
      timeMax = Just . DateTime . zonedTimeToUTC . JST.midnight $ addDays (since + margin') today
      privateExtendedProperties =
        []
      sharedExtendedProperties =
        [ ExtendedProperty ("sh", "bar")
        ]
    calendarEventList <- getCalendarEventList calendarId singleEvents timeMin timeMax orderBy privateExtendedProperties sharedExtendedProperties
    lift $ TonaLogger.logDebug $ display $ tshow calendarEventList

postGmailSendSample :: RIO Config ()
postGmailSendSample =
  TonaGoogle.run errorHandler [ScopeGmailSend] $ do
    GmailSend sentId <- postGmailSend Email
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
    fileResource <- createDriveFileMultipart
      MultipartBody
        { metadata = Metadata
            (Just "tonatona-sample")
            (Just $ MediaType "application/vnd.google-apps.document")
            Nothing
        , mediaType = MediaType "text/plain"
        , mediaContent = MediaContent "This is a sample file created by tonatona-google-server-api."
        }
    lift $ TonaLogger.logDebug $ display ("The file has been successfully created: " <> tshow fileResource)

    list <- getDriveFileList
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
