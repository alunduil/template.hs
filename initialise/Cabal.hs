{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Cabal (convert) where

import Configuration (MetaData (..))
import Control.Exception (Exception)
import Control.Monad.Catch (throwM)
import Data.ByteString (ByteString, readFile)
import Distribution.Fields (Field, readFields)
import Distribution.License (licenseFromSPDX)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec.Error (PError)
import Distribution.Parsec.Position (Position)
import Distribution.SPDX.License (License (License))
import Distribution.SPDX.LicenseExpression (simpleLicenseExpression)
import Distribution.Types.BuildInfo (BuildInfo (hsSourceDirs, otherModules, targetBuildDepends))
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.Executable (Executable (buildInfo, exeName, modulePath))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (packageDescription))
import qualified Distribution.Types.PackageDescription as PD
  ( PackageDescription
      ( author,
        bugReports,
        copyright,
        executables,
        homepage,
        licenseRaw,
        maintainer,
        package,
        sourceRepos,
        testSuites
      ),
  )
import Distribution.Types.PackageId (PackageIdentifier (PackageIdentifier, pkgName, pkgVersion))
import Distribution.Types.PackageName (mkPackageName, unPackageName)
import Distribution.Types.SourceRepo
  ( KnownRepoType (Git),
    RepoKind (RepoHead),
    RepoType (KnownRepoType),
    SourceRepo (repoLocation, repoType),
    emptySourceRepo,
  )
import Distribution.Types.TestSuite (TestSuite (testBuildInfo, testName))
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Distribution.Types.Version (mkVersion)
import Distribution.Utils.ShortText (toShortText)
import GHC.Base (NonEmpty)
import Replace (replaceWith)
import System.FilePath ((</>))
import Text.Parsec.Error (ParseError)
import Prelude hiding (readFile)

instance Exception (NonEmpty PError)

instance Exception ParseError

convert :: MetaData -> FilePath -> IO ()
convert metadata cabal = (path metadata `replaceWith`) . modifyWith metadata <$> contents cabal

contents :: FilePath -> IO [Field Position]
contents path = either throwM pure . readFields =<< readFile path

class ModifyWith a where
  modifyWith :: MetaData -> a -> a

instance ModifyWith GenericPackageDescription where
  modifyWith metadata gpd = gpd {packageDescription = packageDescription'}
    where
      packageDescription' = modifyWith metadata gpd.packageDescription

instance ModifyWith PD.PackageDescription where
  modifyWith metadata pd =
    pd
      { PD.package = modifyWith metadata pd.package,
        PD.licenseRaw = Left . License . simpleLicenseExpression $ metadata.licence,
        PD.copyright = toShortText $ unwords ["(c)", show metadata.year, metadata.author],
        PD.maintainer = toShortText metadata.maintainer,
        PD.author = toShortText metadata.author,
        PD.homepage = toShortText $ show metadata.homepage,
        PD.bugReports = toShortText $ show metadata.homepage ++ "/issues",
        PD.sourceRepos =
          [ (emptySourceRepo RepoHead)
              { repoType = Just $ KnownRepoType Git,
                repoLocation = Just $ show metadata.homepage
              }
          ],
        PD.executables = modifyWith metadata <$> pd.executables,
        PD.testSuites = modifyWith metadata <$> pd.testSuites
      }

instance ModifyWith PackageIdentifier where
  modifyWith metadata _pi =
    PackageIdentifier
      { pkgName = mkPackageName $ name metadata,
        pkgVersion = mkVersion [0, 1, 0, 0]
      }

instance ModifyWith Executable where
  modifyWith metadata executable =
    executable
      { exeName = mkUnqualComponentName metadata.name,
        buildInfo = modifyWith metadata executable.buildInfo
      }

instance ModifyWith TestSuite where
  modifyWith metadata testSuite =
    testSuite
      { testName = mkUnqualComponentName $ metadata.name ++ "-test",
        testBuildInfo = modifyWith metadata testSuite.testBuildInfo
      }

instance ModifyWith BuildInfo where
  modifyWith metadata buildInfo =
    buildInfo
      { otherModules = [],
        targetBuildDepends = filter is_base buildInfo.targetBuildDepends
      }
    where
      is_base = ("base" ==) . unPackageName . depPkgName
