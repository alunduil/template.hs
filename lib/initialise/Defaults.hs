{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Defaults
  ( getDefaults,
    Defaults (..),
    dName,
    dHomePage,
  )
where

import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Time (LocalTime (localDay), getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Calendar (Year)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import qualified Git (config)
import Network.URI (URI (uriPath), parseURI)
import System.Directory.Extra (getCurrentDirectory)
import System.FilePath (dropExtension, takeBaseName)

data Defaults = Defaults
  { dOrigin :: URI,
    dAuthor :: Text,
    dMaintainer :: Text,
    dPath :: FilePath,
    dYear :: Year
  }
  deriving (Show, Eq)

dName :: Defaults -> Text
dName Defaults {..} = pack . takeBaseName . uriPath $ dOrigin

dHomePage :: Defaults -> URI
dHomePage Defaults {..} =
  dOrigin
    { uriPath = dropExtension $ uriPath dOrigin
    }

getDefaults :: IO Defaults
getDefaults = do
  dOrigin <- fromJust . parseURI . unpack <$> Git.config "remote.origin.url"
  dAuthor <- Git.config "user.name"
  dMaintainer <- Git.config "user.email"
  dPath <- getCurrentDirectory
  timezone <- getCurrentTimeZone
  (dYear, _day) <- toOrdinalDate . localDay . utcToLocalTime timezone <$> getCurrentTime
  pure Defaults {..}
