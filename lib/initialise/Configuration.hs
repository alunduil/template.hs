{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Configuration
  ( Configuration (..),
    parser,
  )
where

import Control.Monad.Logger (LogLevel)
import Control.Monad.Logger.CallStack (LogLevel (LevelWarn))
import Data.Text (Text)
import Data.Time.Calendar (Year)
import Defaults (Defaults (..), dCabalName, dHomePage, dName)
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
    cabalName :: Text,
    homepage :: URI,
    author :: Text,
    maintainer :: Text,
    licence :: LicenseId,
    path :: FilePath,
    year :: Year,
    verbosity :: LogLevel
  }

parser :: Defaults -> Parser Configuration
parser ds@(Defaults {..}) = do
  name <-
    strOption
      ( long "name"
          <> help "Name of the new project."
          <> metavar
            "NAME"
          <> maybeDefault (dName ds)
      )
  cabalName <-
    strOption
      ( long "cabal-name"
          <> help "Name to use in cabal."
          <> metavar "CABAL_NAME"
          <> maybeDefault (dCabalName ds)
      )
  homepage <-
    option
      (maybeReader parseURI)
      ( long "homepage"
          <> help
            "Homepage of the new project."
          <> metavar "URL"
          <> maybeDefault (dHomePage ds)
      )
  author <-
    strOption
      ( long "author"
          <> help "Name of the author of the project."
          <> metavar "AUTHOR"
          <> value dAuthor
          <> showDefault
      )
  maintainer <-
    strOption
      ( long "maintainer"
          <> help
            "Email of the maintainer of the project."
          <> metavar "MAINTAINER"
          <> value dMaintainer
          <> showDefault
      )
  licence <-
    option
      auto
      ( long "licence"
          <> help "Licence of the project."
          <> value Unlicense
          <> showDefault
          <> metavar "LICENCE"
      )
  path <-
    option
      auto
      ( long "path"
          <> help "Project path.  Only used for testing."
          <> value dPath
          <> showDefault
          <> metavar "PATH"
          <> hidden
          <> internal
      )
  year <-
    option
      auto
      ( long "year"
          <> help "Copyright year.  Only used for testing."
          <> value dYear
          <> showDefault
          <> metavar "YEAR"
          <> hidden
          <> internal
      )
  verbosity <-
    option
      auto
      ( long "verbosity"
          <> help
            "Verbosity of information printed to stderr."
          <> value LevelWarn
          <> showDefault
          <> metavar "VERBOSITY"
      )
  pure Configuration {..}
  where
    -- maybeDefault :: (HasValue f, Show a) => Maybe a -> Mod f a
    maybeDefault (Just a) = value a <> showDefault
    maybeDefault Nothing = mempty
