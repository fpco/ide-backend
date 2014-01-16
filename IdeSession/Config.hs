module IdeSession.Config (
    SessionConfig(..)
  , InProcess
  , defaultSessionConfig
  ) where

import Distribution.License (License (..))
import Distribution.Simple (PackageDB (..), PackageDBStack)

type InProcess = Bool

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
    -- | Static options for GHC
  , configStaticOpts :: [String]
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
    -- | Include paths (equivalent of GHC's @-i@ parameter) relative to the
    -- temporary directory where we store the session's source files.
    --
    -- By default this is the singleton list @[""]@ -- i.e., we include the
    -- sources dir but nothing else.
  , configRelativeIncludes :: [FilePath]
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
-- >   , configLog              = const $ return ()
-- >   , configDeleteTempFiles  = True
-- >   , configWarnings         = defaultGhcWarnings
-- >   , configRelativeIncludes = [""]
-- >   }
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig {
    configDir              = "."
  , configExtraPathDirs    = []
  , configStaticOpts       = []
  , configInProcess        = False
  , configGenerateModInfo  = True
  , configPackageDBStack   = [GlobalPackageDB, UserPackageDB]
    -- ghc-prim, integer-gmp, etc., all have their own licenses specified
    -- in their .cabal files.
  , configLicenseExc       = ["rts"]
  , configLicenseFixed     =
    [ ("bin-package-db", (Just BSD3, Nothing, Nothing))
    , ("ghc", (Just BSD3, Just "../LICENSE", Just "The GHC Team"))
    , ("ghc-prim", (Just BSD3, Just "LICENSE", Nothing))
    , ("integer-gmp", (Just BSD3, Just "LICENSE", Nothing))
    ]
  , configLog              = const $ return ()
  , configDeleteTempFiles  = True
  , configRelativeIncludes = [""]
  }
