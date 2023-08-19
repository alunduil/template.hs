module Configuration
  ( MetaData (..),
    optionParser,
  )
where

import Data.Maybe (maybe)
import Data.Time.Calendar (Year)
import Distribution.SPDX.LicenseId (LicenseId (Unlicense))
import Network.URI (URI (uriPath), parseURI)
import Options.Applicative
  ( Parser,
    auto,
    help,
    hidden,
    internal,
    long,
    metavar,
    option,
    short,
    showDefault,
    strOption,
    value,
  )
import Options.Applicative.Builder (maybeReader)
import System.FilePath (dropExtension, takeBaseName)

data MetaData = MetaData
  { name :: String,
    homepage :: URI,
    author :: String,
    maintainer :: String,
    licence :: LicenseId,
    path :: FilePath,
    year :: Year
  }

optionParser ::
  Maybe String ->
  Maybe String ->
  Maybe String ->
  FilePath ->
  Year ->
  Parser MetaData
optionParser origin author maintainer path year =
  MetaData
    <$> name'
    <*> homepage'
    <*> author'
    <*> maintainer'
    <*> licence'
    <*> path'
    <*> year'
  where
    name' =
      let ms =
            long "name"
              <> help "Name of the new project."
              <> metavar "NAME"
          withValue n = ms <> value n <> showDefault
       in strOption $ maybe ms withValue $ originToName origin
    homepage' =
      let ms =
            long "homepage"
              <> help "Homepage of the new project."
              <> metavar "URL"
          withValue h = ms <> value h <> showDefault
       in option (maybeReader parseURI) $
            maybe ms withValue $
              originToHomePage origin
    author' =
      let ms =
            long "author"
              <> help "Name of the author of the project."
              <> metavar "AUTHOR"
          withValue a = ms <> value a <> showDefault
       in strOption $ maybe ms withValue author
    maintainer' =
      let ms =
            long "maintainer"
              <> help "Email of the maintainer of the project."
              <> metavar "MAINTAINER"
          withValue m = ms <> value m <> showDefault
       in strOption $ maybe ms withValue maintainer
    licence' =
      option auto $
        long "licence"
          <> help "Licence of the project."
          <> value Unlicense
          <> showDefault
          <> metavar "LICENCE"

    path' =
      option auto $
        long "path"
          <> help "Project path.  Only used for testing."
          <> value path
          <> showDefault
          <> metavar "PATH"
          <> hidden
          <> internal

    year' =
      option auto $
        long "year"
          <> help "Copyright year.  Only used for testing."
          <> value year
          <> showDefault
          <> metavar "YEAR"
          <> hidden
          <> internal

originToName :: Maybe String -> Maybe String
originToName origin = takeBaseName . uriPath <$> (parseURI =<< origin)

originToHomePage :: Maybe String -> Maybe URI
originToHomePage origin = dropExtension' <$> (parseURI =<< origin)
  where
    dropExtension' uri = uri {uriPath = dropExtension $ uriPath uri}
