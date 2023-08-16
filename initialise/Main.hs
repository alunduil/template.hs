module Main (main) where

import Configuration
  ( MetaData,
    optionParser,
  )
import qualified Git (config)
import Options.Applicative
  ( execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
  )

-- TODO Add logging

main :: IO ()
main = do
  author <- Git.config "user.name"
  metadata <- execParser $ options author
  pure ()
  where
    options author =
      info
        (optionParser author <**> helper)
        ( fullDesc
            <> progDesc "Initialise a new project using the current checked out repository."
            <> header "WARNING: THIS WILL DESTROY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
        )
