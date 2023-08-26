module Main (main, main') where

import qualified Cabal (convert)
import qualified Configuration
  ( MetaData (path),
    optionParser,
  )
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Time (UTCTime (utctDay), toGregorian)
import Data.Time.Clock (getCurrentTime)
import qualified Defaults (getDefaults)
import qualified File (convert)
import qualified Git (config)
import Initialise (Initialise)
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

main :: IO ()
main = do
  defaults <- Defaults.getDefaults

  let options =
        info
          (Configuration.parser defaults <**> helper)
          ( fullDesc
              <> progDesc
                ( unlines
                    [ "Initialise a new project using the current checked out repository.",
                      "WARNING: THIS WILL MODIFY THE CURRENT CONTENTS OF YOUR CHECKED OUT REPOSITORY!"
                    ]
                )
          )

  execParser options >>= runInitialise main'

main' :: Initialise ()
main' = do
  unlessM (Licence.is Unlicense) $ Licence.replace "LICENSE"
  Cabal.replace "templatise.cabal"
  mapM_
    File.replace
    [ ".devcontainer" </> "devcontainer.json",
      "CHANGELOG.md"
    ]

-- TODO README
