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
    maintainer :: String
  }

optionParser :: Maybe String -> Maybe String -> Parser MetaData
optionParser author maintainer =
  MetaData <$> name' <*> author' <*> maintainer'
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
