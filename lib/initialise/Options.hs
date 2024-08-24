{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Options (parserInfo) where

import Control.Monad.Logger.CallStack (LogLevel (LevelWarn))
import Defaults (Defaults (..), dCabalName, dHomePage, dName)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense))
import qualified Environment (T (..))
import Network.URI (parseURI)
import Options.Applicative
  ( HasValue,
    Mod,
    Parser,
    ParserInfo,
    auto,
    fullDesc,
    help,
    helper,
    hidden,
    info,
    internal,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    value,
    (<**>),
  )
import Options.Applicative.Builder (maybeReader)

parserInfo :: Defaults -> ParserInfo Environment.T
parserInfo ds =
  info
    (parser ds <**> helper)
    ( fullDesc
        <> progDesc
          ( unlines
              [ "Initialise a new project using the current checked out repository.",
                "WARNING: THIS WILL MODIFY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
              ]
          )
    )

parser :: Defaults -> Parser Environment.T
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
  pure Environment.T {..}

maybeDefault :: (HasValue f, Show a) => Maybe a -> Mod f a
maybeDefault (Just a) = value a <> showDefault
maybeDefault Nothing = mempty
