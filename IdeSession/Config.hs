module IdeSession.Config (
    SessionConfig(..)
  , defaultSessionConfig
  ) where

import IdeSession.GHC.Client (InProcess)

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {
    -- | The directory to use for all session files.
    configDir        :: FilePath
    -- | GHC static options. Can also contain default dynamic options,
    -- that are overridden via session update.
  , configStaticOpts :: [String]
    -- | Should the GHC client run in-process?
    -- NOTE: This is currently broken. Set to False.
  , configInProcess  :: InProcess
    -- Whether to generate module type/autocompletion info.
  , configGenerateModInfo :: Bool
    -- Build shared libraries and dynamically link executables.
  , configDynLink :: Bool
    -- Package dbs to consult. Assumes global and user dbs, if @Nothing@.
  , configPackageDBStack :: Maybe [FilePath]
    -- Packages that don't need the .cabal files provided for license
    -- concatenation (e.g., because they are covered by the core license set).
  , configLicenseExc :: [String]
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
-- >   }
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig {
    configDir             = "."
  , configStaticOpts      = []
  , configInProcess       = False
  , configGenerateModInfo = True
  , configDynLink         = False
  , configPackageDBStack  = Nothing
    -- ghc-prim, integer-gmp, etc., all have their own licenses specified
    -- in their .cabal files.
  , configLicenseExc      = ["rts"]
  }
