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
import System.Directory.Extra (getCurrentDirectory)

-- TODO Add logging

main :: IO ()
main = do
  author <- Git.config "user.name"
  maintainer <- Git.config "user.email"
  path <- getCurrentDirectory

  metadata <- execParser $ options author maintainer path

  pure ()
  where
    options author maintainer path =
      info
        (optionParser author maintainer path <**> helper)
        ( fullDesc
            <> progDesc
              ( unlines
                  [ "Initialise a new project using the current checked out repository.",
                    "WARNING: THIS WILL DESTROY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
                  ]
              )
        )
