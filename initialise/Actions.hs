module Actions (convertLicence, convertCabal, convertFile) where

import Configuration (MetaData)

convertLicence :: MetaData -> FilePath -> IO ()
convertLicence metadata licence = undefined

convertCabal :: MetaData -> FilePath -> IO ()
convertCabal metadata cabal = undefined

convertFile :: MetaData -> FilePath -> IO ()
convertFile metadata file = undefined
