module Main (main, main') where

import Actions (convertCabal, convertFile, convertLicence)
import Configuration
  ( MetaData (path),
    optionParser,
    toTemplate,
  )
import Control.Monad (forM_)
import qualified Git (config)
import Network.URI (URI (uriPath), parseURI)
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
import System.FilePath (takeBaseName, (</>))

-- TODO Add logging

main :: IO ()
main = do
  name <- toName <$> Git.config "remote.origin.url"
  author <- Git.config "user.name"
  maintainer <- Git.config "user.email"
  path <- getCurrentDirectory

  execParser (options name author maintainer path) >>= main'
  where
    options name author maintainer path =
      info
        (optionParser name author maintainer path <**> helper)
        ( fullDesc
            <> progDesc
              ( unlines
                  [ "Initialise a new project using the current checked out repository.",
                    "WARNING: THIS WILL DESTROY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
                  ]
              )
        )

    toName :: Maybe String -> Maybe String
    toName origin = takeBaseName . uriPath <$> (parseURI =<< origin)

main' :: MetaData -> IO ()
main' metadata = do
  convertLicence metadata (path metadata </> "LICENSE")
  convertCabal metadata (path metadata </> "templatise.cabal")

  let files = [path metadata </> ".devcontainer" </> "devcontainer.json"]

  forM_ files $ convertFile metadata

-- TODO Cabal source-dirs need converting.  Put it in convertCabal?
