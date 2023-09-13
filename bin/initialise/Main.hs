module Main (main, main') where

import qualified Cabal (replace)
import qualified Configuration (parser)
import qualified Defaults (getDefaults)
import qualified File (replace)
import Initialise (Initialise, runInitialise)
import qualified Licence (replace)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc, (<**>))
import System.FilePath ((</>))

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
  mapM_
    File.replace
    [ ".devcontainer" </> "devcontainer.json",
      ".github" </> "workflows" </> "haskell-ci.yml",
      "CHANGELOG.md"
    ]
  Licence.replace "LICENSE"
  Cabal.replace "templatise.cabal"

-- TODO README
