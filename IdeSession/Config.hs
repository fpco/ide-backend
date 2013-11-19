module IdeSession.Config (
    SessionConfig(..)
  , defaultSessionConfig
  ) where

import Distribution.Simple (PackageDB(..), PackageDBStack)
import Distribution.License (License (..))

import IdeSession.GHC.Client (InProcess)

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {
    -- | The directory to use for all session files.
    configDir        :: FilePath
    -- | Extra directories in which to look for programs, including ghc
    -- and other tools. Note that the @$PATH@ is still searched /first/, these
    -- directories are extra.
  , configExtraPathDirs :: [FilePath]
    -- | GHC static options. Can also contain default dynamic options,
    -- that are overridden via session update.
  , configStaticOpts :: [String]
    -- | Should the GHC client run in-process?
    -- NOTE: This is currently broken. Set to False.
  , configInProcess  :: InProcess
    -- | Whether to generate module type/autocompletion info.
  , configGenerateModInfo :: Bool
    -- | Build shared libraries and dynamically link executables.
  , configDynLink :: Bool
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
  }

-- | Default session configuration
--
-- Use this instead of creating your own SessionConfig to be robust against
-- extensions of SessionConfig.
--
-- > defaultSessionConfig = SessionConfig {
-- >     configDir             = "."
-- >   , configStaticOpts      = []
-- >   , configInProcess       = False
-- >   , configGenerateModInfo = True
-- >   , configDynLink         = False
-- >   , configPackageDBStack  = [GlobalPackageDB, UserPackageDB]
-- >   , configLicenseExc      = ["rts"]
-- >   , configLicenseFixed    =
-- >     [ ("bin-package-db", (Just BSD3, Nothing, Nothing))
-- >     , ("ghc", (Just BSD3, Just "../LICENSE", Just "The GHC Team"))
-- >     , ("ghc-prim", (Just BSD3, Just "LICENSE", Nothing))
-- >     , ("integer-gmp", (Just BSD3, Just "LICENSE", Nothing))
-- >     ]
-- >   }
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig {
    configDir             = "."
  , configExtraPathDirs   = []
  , configStaticOpts      = []
  , configInProcess       = False
  , configGenerateModInfo = True
  , configDynLink         = False
  , configPackageDBStack  = [GlobalPackageDB, UserPackageDB]
    -- ghc-prim, integer-gmp, etc., all have their own licenses specified
    -- in their .cabal files.
  , configLicenseExc      = ["rts"]
  , configLicenseFixed    =
    [ ("bin-package-db", (Just BSD3, Nothing, Nothing))
    , ("ghc", (Just BSD3, Just "../LICENSE", Just "The GHC Team"))
    , ("ghc-prim", (Just BSD3, Just "LICENSE", Nothing))
    , ("integer-gmp", (Just BSD3, Just "LICENSE", Nothing))
    ]
  , configLog             = const $ return ()
  , configDeleteTempFiles = True
  }
