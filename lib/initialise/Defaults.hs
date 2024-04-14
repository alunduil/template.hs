{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Defaults
  ( getDefaults,
    Defaults (..),
    dName,
    dHomePage,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, replace, stripPrefix, unpack)
import Data.Time (LocalTime (localDay), getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Calendar (Year)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import qualified Git (config)
import Network.URI (URI (uriPath), parseURI)
import System.Directory.Extra (getCurrentDirectory)
import System.FilePath (stripExtension, takeFileName)

data Defaults = Defaults
  { dOrigin :: Text,
    dAuthor :: Text,
    dMaintainer :: Text,
    dPath :: FilePath,
    dYear :: Year
  }
  deriving (Show, Eq)

dName :: Defaults -> Maybe Text
dName = fmap toName . dHomePage
  where
    toName = pack . takeFileName . uriPath

dHomePage :: Defaults -> Maybe URI
dHomePage Defaults {..} =
  stripURIExtension "git"
    <$> ( toURI dOrigin
            <|> (toURI =<< sshToHttp dOrigin)
        )
  where
    sshToHttp = fmap (("http://" <>) . replace ":" "/") . stripPrefix "git@"
    toURI = parseURI . unpack
    stripURIExtension ext uri =
      let p = uriPath uri
       in uri {uriPath = fromMaybe p (stripExtension ext p)}

getDefaults :: IO Defaults
getDefaults = do
  dOrigin <- Git.config "remote.origin.url"
  dAuthor <- Git.config "user.name"
  dMaintainer <- Git.config "user.email"
  dPath <- getCurrentDirectory
  timezone <- getCurrentTimeZone
  (dYear, _day) <- toOrdinalDate . localDay . utcToLocalTime timezone <$> getCurrentTime
  pure Defaults {..}
