module IdeSession.Config (
    SessionConfig(..)
  ) where

import IdeSession.GHC.Server (InProcess) -- Should this be someplace else?

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
  , configInProcess  :: InProcess
    -- Whether to generate module type/autocompletion info.
  , configGenerateModInfo :: Bool
  }
