{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration
  ( Configuration (..),
    parser,
  )
where

import Data.Text (Text)
import Data.Time.Calendar (Year)
import Defaults (Defaults (..), dHomePage, dName)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense))
import Network.URI (URI, parseURI)
import Options.Applicative
  ( Parser,
    auto,
    help,
    hidden,
    internal,
    long,
    metavar,
    option,
    showDefault,
    strOption,
    value,
  )
import Options.Applicative.Builder (maybeReader)

data Configuration = Configuration
  { name :: Text,
    homepage :: URI,
    author :: Text,
    maintainer :: Text,
    licence :: LicenseId,
    path :: FilePath,
    year :: Year
  }

parser :: Defaults -> Parser Configuration
parser ds@(Defaults {..}) =
  Configuration
    <$> strOption
      ( long "name"
          <> help "Name of the new project."
          <> metavar "NAME"
          <> value (dName ds)
          <> showDefault
      )
    <*> option
      (maybeReader parseURI)
      ( long "homepage"
          <> help "Homepage of the new project."
          <> metavar "URL"
          <> value (dHomePage ds)
          <> showDefault
      )
    <*> strOption
      ( long "author"
          <> help "Name of the author of the project."
          <> metavar "AUTHOR"
          <> value dAuthor
          <> showDefault
      )
    <*> strOption
      ( long "maintainer"
          <> help "Email of the maintainer of the project."
          <> metavar "MAINTAINER"
          <> value dMaintainer
          <> showDefault
      )
    <*> option
      auto
      ( long "licence"
          <> help "Licence of the project."
          <> value Unlicense
          <> showDefault
          <> metavar "LICENCE"
      )
    <*> option
      auto
      ( long "path"
          <> help "Project path.  Only used for testing."
          <> value dPath
          <> showDefault
          <> metavar "PATH"
          <> hidden
          <> internal
      )
    <*> option
      auto
      ( long "year"
          <> help "Copyright year.  Only used for testing."
          <> value dYear
          <> showDefault
          <> metavar "YEAR"
          <> hidden
          <> internal
      )
