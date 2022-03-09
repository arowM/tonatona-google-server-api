module TonaApp.JST (getCurrentDay, midnight, zonedTime) where

import Data.Fixed (Pico)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.LocalTime
  ( LocalTime (..),
    TimeOfDay (..),
    TimeZone (..),
    ZonedTime (..),
    localDay,
    utcToZonedTime,
  )
import Tonalude

jst :: TimeZone
jst = TimeZone 540 False "JST"

-- | Get the current time in the JST timezone as a 'ZonedTime'.
getCurrentTimeJST :: MonadIO m => m ZonedTime
getCurrentTimeJST =
  liftIO $ utcToJST <$> getCurrentTime

getCurrentDay :: MonadIO m => m Day
getCurrentDay =
  liftIO $ localDay . zonedTimeToLocalTime <$> getCurrentTimeJST

zonedTime :: Day -> Int -> Int -> Pico -> ZonedTime
zonedTime day hour minute pico = ZonedTime (LocalTime day (TimeOfDay hour minute pico)) jst

-- | Convert UTCTime to JST timezone as a 'ZonedTime'.
utcToJST :: UTCTime -> ZonedTime
utcToJST = utcToZonedTime jst

-- | Get midnight of given day in the JST timezone as a 'ZonedTime'.
midnight :: Day -> ZonedTime
midnight day = zonedTime day 0 0 0
