module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Google as TonaGoogle



-- App


app :: RIO Config ()
app = do
  -- Tonatona.Logger plugin enables to use logger functions without any configurations.
  TonaLogger.logInfo $ display ("This is a sample project for tonatona-google-server-api" :: Text)
  TonaLogger.logDebug $ display ("This is a debug message" :: Text)



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  -- , anotherPlugin :: TonaAnotherPlugin.Config
  -- , yetAnotherPlugin :: TonaYetAnotherPlugin.Config
  }


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger


instance HasParser Config where
  parser = Config
      <$> parser
      -- <*> parser
      -- <*> parser
