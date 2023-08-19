module Main (main, main') where

import qualified Cabal (convert)
import Configuration
  ( MetaData (path),
    optionParser,
  )
import Control.Monad (forM_)
import Data.Time (UTCTime (utctDay), toGregorian)
import Data.Time.Clock (getCurrentTime)
import qualified File (convert)
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
import System.FilePath (dropExtension, takeBaseName, (</>))

-- TODO Add logging

main :: IO ()
main = do
  origin <- Git.config "remote.origin.url"
  author <- Git.config "user.name"
  maintainer <- Git.config "user.email"
  path <- getCurrentDirectory
  (year, _month, _day) <- toGregorian . utctDay <$> getCurrentTime

  let options =
        info
          (optionParser origin author maintainer path year <**> helper)
          ( fullDesc
              <> progDesc
                ( unlines
                    [ "Initialise a new project using the current checked out repository.",
                      "WARNING: THIS WILL MODIFY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
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
  Cabal.convert metadata (path metadata </> "templatise.cabal")
  forM_ files $ File.convert metadata
  where
    files =
      [ path metadata </> ".devcontainer" </> "devcontainer.json",
        path metadata </> "CHANGELOG.md"
      ]
