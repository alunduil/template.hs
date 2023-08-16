module Configuration
  ( MetaData,
    optionParser,
  )
where

import Options.Applicative
  ( Parser,
    help,
    long,
    metavar,
    short,
    showDefault,
    strOption,
  )

newtype MetaData = MetaData {name :: String}

optionParser :: Parser MetaData
optionParser =
  MetaData
    <$> strOption
      ( short 'n'
          <> long "project-name"
          <> help "Name of the new project."
          <> showDefault
          <> metavar "NAME"
      )
