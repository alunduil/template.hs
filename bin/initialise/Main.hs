module Main (main, main') where

import qualified Cabal (replace)
import qualified Configuration (parser)
import qualified Defaults (getDefaults)
import qualified File (replace)
import Initialise (Initialise)
import qualified Licence (is, replace)

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
