module IdeSession where

data IdeSession

data SessionConfig = SessionConfig {
       
       configSourcesDir :: FilePath,
       configWorkingDir :: FilePath,
       configTempDir    :: FilePath,
       -- ...
     }

-- Responsibilty for managing and mutating files in the sources dir.
--
-- In general, updating and changing source files in the sources dir has to
-- be coordinated with the IdeSession, since we're in a concurrent mutable
-- setting.
--
-- The compromise is that the caller can manage the files prior to initialising
-- a session, but while the session is active, all file changes must be managed
-- via the session, and sequenced relative to other session state changes.
--

init :: SessionConfig -> [(ModuleName, FilePath)] -> IO IdeSession

shutdown :: IdeSession -> IO ()

updateModules :: IdeSession -> [ModuleChange] -> IO ()

data ModuleChange = PutModule    ModuleName ByteString
                  | DeleteModule ModuleName


getErrors :: IdeSession -> IO [SourceError]

data SourceError


-- | A mapping from symbol uses to symbol definitions
--
data SymbolDefinitionMap

getSymbolDefinitionMap
