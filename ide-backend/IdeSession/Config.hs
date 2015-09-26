module IdeSession.Config (
    SessionConfig(..)
  , InProcess
  , ProgramSearchPath, ProgramSearchPathEntry(..)
  , sessionConfigFromEnv
  , defaultSessionConfig
  ) where

import Distribution.License (License (..))
import Distribution.Simple (PackageDB (..), PackageDBStack)
import Distribution.Simple.Program.Find (ProgramSearchPath,ProgramSearchPathEntry(..),defaultProgramSearchPath)
import System.FilePath (splitSearchPath)
import System.Directory (getCurrentDirectory, getTemporaryDirectory)
import System.Environment (lookupEnv)

type InProcess = Bool

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {
    -- | The directory to use for all session files.
    configDir        :: FilePath
    -- | When set to Just "<filepath>", we'll use the files in that
    -- directory as source and datafiles.  This means that the
    -- ide-backend is no longer directly managing the files, and
    -- file updates like 'updateSourceFile' will fail.
    --
    -- Note that this feature is experimental and does not have a
    -- suite of tests.
    --
    -- Since this is likely used with an existing cabal project, which
    -- might have multiple source directories, you'll likely want to
    -- use 'TargetsInclude' instead of 'TargetsExclude'.
  , configLocalWorkingDir :: Maybe FilePath
    -- | Extra directories in which to look for programs, including ghc
    -- and other tools. Note that the @$PATH@ is still searched /first/, these
    -- directories are extra.
  , configExtraPathDirs :: [FilePath]
    -- | Should the GHC client run in-process?
    -- NOTE: This is currently broken. Set to False.
  , configInProcess  :: InProcess
    -- | Whether to generate module type/autocompletion info.
  , configGenerateModInfo :: Bool
    -- | Package DBs to consult
  , configPackageDBStack :: PackageDBStack
    -- | Packages that don't need the .cabal files provided for license
    -- concatenation (e.g., because they are covered by the core license set).
  , configLicenseExc :: [String]
    -- | Hard-coded package licence information, e.g., for the packages
    -- that always stay installed in-place in the GHC tree, so it's
    -- troublesome to automatically retrieve their .cabal files.
  , configLicenseFixed :: [( String
                           , (Maybe License, Maybe FilePath, Maybe String)
                           )]
    -- | Function to be used for logging. Messages logged in this manner may be
    -- provided to users in a special debugging UI.
  , configLog :: String -> IO ()
    -- | Delete temporary files when session finishes?
    -- (Defaults to True; mostly for internal debugging purposes)
  , configDeleteTempFiles :: Bool
    -- | The name of the ide-backend-server program to use, and where to find it.
    --   The default is @(defaultProgramSearchPath,"ide-backend-server")@,
    --   that is, to look for a program called ide-backend-server on the system
    --   search path only.
  , configIdeBackendServer :: (ProgramSearchPath,FilePath)
    -- | The name of the ide-backend-exe-cabal program to use, and where to find it.
    --   The default is @(defaultProgramSearchPath,"ide-backend-exe-cabal")@.
  , configIdeBackendExeCabal :: (ProgramSearchPath,FilePath)
  }

-- | Get the default local session configuration, pulling the following
-- information from the environment:
--
-- * OS temporary directory (used for session.* files)
--
-- * Current working directory - assumed to be the project root. Instead
--   of sending files in updates, they are read from the filesystem.
--
-- * GHC package database.  Like GHC, it takes the GHC_PACKAGE_PATH,
--   and uses this list of package databases.  This allows ide-backend
--   to do the 'right thing' when used with tools like stack.
sessionConfigFromEnv :: IO SessionConfig
sessionConfigFromEnv = do
  tmpDir <- getTemporaryDirectory
  cwd <- getCurrentDirectory
  mpkgPath <- lookupEnv "GHC_PACKAGE_PATH"
  let dbStack = case mpkgPath of
        Nothing -> configPackageDBStack defaultSessionConfig
        Just pkgPath ->
          let dbPaths = drop 1 (reverse (splitSearchPath pkgPath))
           in GlobalPackageDB : map SpecificPackageDB dbPaths
  return defaultSessionConfig
    { configDir = tmpDir
    , configLocalWorkingDir = Just cwd
    , configPackageDBStack = dbStack
    }

-- | Default session configuration.  Most users will probably instead
-- want 'localSessionConfigFromEnv'.
--
-- Use this instead of creating your own SessionConfig to be robust
-- against extensions of SessionConfig.
--
-- > defaultSessionConfig = SessionConfig {
-- >     configDir              = "."
-- >   , configLocalWorkingDir  = Nothing
-- >   , configExtraPathDirs    = []
-- >   , configInProcess        = False
-- >   , configGenerateModInfo  = True
-- >   , configPackageDBStack   = [GlobalPackageDB, UserPackageDB]
-- >     -- ghc-prim, integer-gmp, etc., all have their own licenses specified
-- >     -- in their .cabal files.
-- >   , configLicenseExc       = ["rts"]
-- >   , configLicenseFixed     = [
-- >         ("bin-package-db", (Just BSD3, Nothing,           Nothing))
-- >       , ("ghc",            (Just BSD3, Just "../LICENSE", Just "The GHC Team"))
-- >       , ("ghc-prim",       (Just BSD3, Just "LICENSE",    Nothing))
-- >       , ("integer-gmp",    (Just BSD3, Just "LICENSE",    Nothing))
-- >       ]
-- >   , configLog              = const $ return ()
-- >   , configDeleteTempFiles  = True
-- >   , configIdeBackendServer = (defaultProgramSearchPath,"ide-backend-server")
-- >   , configIdeBackendExeCabal = (defaultProgramSearchPath,"ide-backend-exe-cabal")
-- >   }
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig {
    configDir              = "."
  , configLocalWorkingDir  = Nothing
  , configExtraPathDirs    = []
  , configInProcess        = False
  , configGenerateModInfo  = True
  , configPackageDBStack   = [GlobalPackageDB, UserPackageDB]
    -- ghc-prim, integer-gmp, etc., all have their own licenses specified
    -- in their .cabal files.
  , configLicenseExc       = ["rts"]
  , configLicenseFixed     = [
        ("bin-package-db", (Just BSD3, Nothing,           Nothing))
      , ("ghc",            (Just BSD3, Just "../LICENSE", Just "The GHC Team"))
      , ("ghc-prim",       (Just BSD3, Just "LICENSE",    Nothing))
      , ("integer-gmp",    (Just BSD3, Just "LICENSE",    Nothing))
      ]
  , configLog              = const $ return ()
  , configDeleteTempFiles  = True
  , configIdeBackendServer = (defaultProgramSearchPath,"ide-backend-server")
  , configIdeBackendExeCabal = (defaultProgramSearchPath,"ide-backend-exe-cabal")
  }
