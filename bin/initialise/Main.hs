module Main (main) where

import qualified Defaults (getDefaults)
import Initialiser (defaultInitialiser, runInitialiser)
import qualified Options (parserInfo)
import Options.Applicative (execParser)

main :: IO ()
main = Defaults.getDefaults >>= execParser . Options.parserInfo >>= runInitialiser defaultInitialiser
