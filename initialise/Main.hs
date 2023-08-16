module Main (main) where

import Project (MetaData, optionParser)

-- TODO Add logging

main :: IO ()
main = do
  metadata <- execParser options
  putStrLn "Hello, Haskell!"
  where
    options =
      info
        (optionParser <**> helper)
        ( fullDesc
            <> progDesc "Initialise a new project using the current checked out repository."
            <> header "WARNING: THIS WILL DESTROY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
        )
