{- |
Module      :  Tonatona.Google.Client

Define functions to call Google APIs.
-}
module Tonatona.Google.Client
  ( postCalendarEvent
  , postGmailSend
  ) where

import Tonalude
import qualified Google.Client as Client
import qualified Google.Form as Form
import qualified Google.Response as Response
import Tonatona.Google.Internal

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
