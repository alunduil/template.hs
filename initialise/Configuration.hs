module Configuration
  ( MetaData,
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

data MetaData = MetaData {name :: String, author :: String}

optionParser :: Maybe String -> Parser MetaData
optionParser author =
  MetaData
    <$> strOption
      ( short 'n'
          <> long "project-name"
          <> help "Name of the new project."
          <> showDefault
          <> metavar "NAME"
      )
    <*> let ms =
              ( long "author"
                  <> help "Name of the author of the project."
                  <> metavar "AUTHOR"
              )
            withValue a =
              ms
                <> value a
                <> showDefault
         in strOption $ maybe ms withValue author
