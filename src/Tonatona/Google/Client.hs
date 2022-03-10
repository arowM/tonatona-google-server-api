{- |
Module      :  Tonatona.Google.Client

Define functions to call Google APIs.
-}
module Tonatona.Google.Client
  ( getCalendarEventList
  , postCalendarEvent
  , postGmailSend
  , getDriveFileList
  , createDriveFileMultipart
  , downloadDriveFile
  ) where

import Tonalude
import qualified Google.Client as Client
import qualified Google.Form as Form
import qualified Google.Response as Response
import Tonatona.Google.Internal


getCalendarEventList ::
     Text
  -> Maybe Bool
  -> Maybe Form.DateTime
  -> Maybe Form.DateTime
  -> Maybe Text
  -> [Form.ExtendedProperty] -- ^ private extended properties
  -> [Form.ExtendedProperty] -- ^ shared extended properties
  -> Dsl env Response.CalendarEventList
getCalendarEventList calendarId singleEvents timeMin timeMax orderBy privateExtendedProperty sharedExtendedProperty = do
  t <- asks token
  res <- liftIO $ Client.getCalendarEventList
    t
    calendarId
    singleEvents
    timeMin
    timeMax
    orderBy
    privateExtendedProperty
    sharedExtendedProperty
  case res of
    Left err -> throwM err
    Right resp -> pure resp

postCalendarEvent :: Form.CalendarEvent -> Dsl env Response.CalendarEvent
postCalendarEvent event = do
  t <- asks token
  res <- liftIO $ Client.postCalendarEvent t event
  case res of
    Left err -> throwM err
    Right resp -> pure resp

postGmailSend :: Form.Email -> Dsl env Response.GmailSend
postGmailSend email = do
  t <- asks token
  res <- liftIO $ Client.postGmailSend t email
  case res of
    Left err -> throwM err
    Right resp -> pure resp

getDriveFileList ::
     Form.GetFileParams
  -> Dsl env Response.FileList
getDriveFileList params = do
  t <- asks token
  res <- liftIO $ Client.getDriveFileList t params
  case res of
    Left err -> throwM err
    Right resp -> pure resp

createDriveFileMultipart ::
     Form.MultipartBody
  -> Dsl env Response.FileResource
createDriveFileMultipart body = do
  t <- asks token
  res <- liftIO $ Client.createDriveFileMultipart t body
  case res of
    Left err -> throwM err
    Right resp -> pure resp

downloadDriveFile ::
     Form.DownloadFileParams
  -> Dsl env Response.MediaContent
downloadDriveFile params = do
  t <- asks token
  res <- liftIO $ Client.downloadDriveFile t params
  case res of
    Left err -> throwM err
    Right resp -> pure resp
