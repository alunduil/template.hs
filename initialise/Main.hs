module Main (main, main') where

import Actions (convertCabal, convertFile)
import Configuration
  ( MetaData (path),
    optionParser,
  )
import Control.Monad (forM_)
import qualified Git (config)
import qualified Licence (convert)
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

  let options =
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

  execParser options >>= main'
  where
    toName :: Maybe String -> Maybe String
    toName origin = takeBaseName . uriPath <$> (parseURI =<< origin)

main' :: MetaData -> IO ()
main' metadata = do
  Licence.convert metadata (path metadata </> "LICENSE")
  -- TODO Cabal source-dirs need converting.  Put it in convertCabal?
  convertCabal metadata (path metadata </> "templatise.cabal")
  forM_ files $ convertFile metadata
  where
    files =
      [ path metadata </> ".devcontainer" </> "devcontainer.json",
        path metadata </> "CHANGELOG.md"
      ]
