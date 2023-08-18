module Actions (convertLicence, convertCabal, convertFile) where

import Configuration (MetaData)
import Licence (text)

convertLicence :: MetaData -> FilePath -> IO ()
convertLicence MetaData {licence = "Unlicense"} _path = pure ()
convertLicence MetaData {licence = licence} path = writeFile path $ text licence

convertCabal :: MetaData -> FilePath -> IO ()
convertCabal metadata cabal = undefined

convertFile :: MetaData -> FilePath -> IO ()
convertFile metadata file = undefined
