module Configuration
  ( MetaData (..),
    optionParser,
  )
where

import Data.Maybe (maybe)
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

data MetaData = MetaData
  { name :: String,
    author :: String,
    maintainer :: String,
    licence :: String,
    path :: FilePath
  }

-- TODO prompt for name and licence.

optionParser :: Maybe String -> Maybe String -> FilePath -> Parser MetaData
optionParser author maintainer path =
  MetaData <$> name' <*> author' <*> maintainer' <*> licence' <*> path'
  where
    name' =
      strOption
        ( short 'n'
            <> long "project-name"
            <> help "Name of the new project."
            <> showDefault
            <> metavar "NAME"
        )
    author' =
      let ms =
            ( long "author"
                <> help "Name of the author of the project."
                <> metavar "AUTHOR"
            )
          withValue a = ms <> value a <> showDefault
       in strOption $ maybe ms withValue author
    maintainer' =
      let ms =
            ( long "maintainer"
                <> help "Email of the maintainer of the project."
                <> metavar "MAINTAINER"
            )
          withValue m = ms <> value m <> showDefault
       in strOption $ maybe ms withValue maintainer
    licence' =
      strOption
        ( long "licence"
            <> help "Licence of the project."
            <> value "unlicence"
            <> showDefault
            <> metavar "LICENCE"
        )
    path' =
      option
        auto
        ( long "path"
            <> help "Project path.  Only used for testing."
            <> value path
            <> showDefault
            <> metavar "PATH"
            <> hidden
            <> internal
        )

projectName :: String -> String
projectName original = undefined
