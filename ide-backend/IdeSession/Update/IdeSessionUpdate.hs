-- | Declarative description of session updates
--
-- We separate the declarative _description_ of a session update from its
-- imperative _execution_. This means that the order in which we execute
-- session updates is independent of the order of the specification, and
-- moreover we can analyze an entire session update to see if we need to
-- restart the s session in order to execute it.
module IdeSession.Update.IdeSessionUpdate (
    -- * Data types describing session updates
    IdeSessionUpdate(..)
  , FileInfo(..)
  , FileCmd(..)
    -- * Individual updates
  , updateDeleteManagedFiles
  , updateSourceFile
  , updateSourceFileFromFile
  , updateSourceFileDelete
  , updateDataFile
  , updateDataFileFromFile
  , updateDataFileDelete
  , updateGhcOpts
  , updateRtsOpts
  , updateRelativeIncludes
  , updateCodeGeneration
  , updateEnv
  , updateArgs
  , updateStdoutBufferMode
  , updateStderrBufferMode
  , updateTargets
  , buildExe
  , buildDoc
  , buildLicenses
  ) where

import Prelude hiding (mod, span)
import Control.Monad (mplus)
import Data.Accessor (Accessor)
import Data.Monoid (Monoid(..))
import qualified Data.ByteString.Lazy.Char8 as BSL

import IdeSession.State
import IdeSession.Types.Private hiding (RunResult(..))
import IdeSession.Types.Public (RunBufferMode(..))
import qualified IdeSession.Types.Public as Public

{-------------------------------------------------------------------------------
  Data types
-------------------------------------------------------------------------------}

-- | Declarative description of session updates
--
-- IdeSessionUpdate forms a monoid, which is right-biased: "later" calls
-- override "earlier" ones:
--
-- > updateTargets targets1 <> updateTargets2
--
-- is equivalent to
--
-- > updateTargets2
--
-- However, updates of a different nature are not necessarily executed in order;
-- for instance,
--
-- > updateDynamicOpts opts <> updateSourceFile fp bs
--
-- is equivalent to
--
-- > updateSourceFile fp bs <> updateDynamicOpts opts
--
-- In both cases options are set before new source files are compiled.
--
-- File commands are updated in order, so that
--
-- > updateSourceFile fp bs <> updateSourceFile fp bs'
--
-- is equivalent to
--
-- > updateSourceFile fp bs'
--
-- which is consistent with "later updates override earlier ones".
data IdeSessionUpdate = IdeSessionUpdate {
    ideUpdateFileCmds    :: [FileCmd]
  , ideUpdateDeleteFiles :: Bool
  , ideUpdateGhcOpts     :: Maybe [String]
  , ideUpdateRelIncls    :: Maybe [FilePath]
  , ideUpdateCodeGen     :: Maybe Bool
  , ideUpdateEnv         :: Maybe [(String, Maybe String)]
  , ideUpdateArgs        :: Maybe [String]
  , ideUpdateStdoutMode  :: Maybe RunBufferMode
  , ideUpdateStderrMode  :: Maybe RunBufferMode
  , ideUpdateTargets     :: Maybe Public.Targets
  , ideUpdateExes        :: [([String], [(ModuleName, FilePath)])]
  , ideUpdateDocs        :: Bool
  , ideUpdateLicenses    :: [FilePath]
  , ideUpdateRtsOpts     :: Maybe [String]
  }
  deriving Show

instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate {
      ideUpdateFileCmds    = []
    , ideUpdateDeleteFiles = False
    , ideUpdateGhcOpts     = Nothing
    , ideUpdateRelIncls    = Nothing
    , ideUpdateCodeGen     = Nothing
    , ideUpdateEnv         = Nothing
    , ideUpdateArgs        = Nothing
    , ideUpdateStdoutMode  = Nothing
    , ideUpdateStderrMode  = Nothing
    , ideUpdateTargets     = Nothing
    , ideUpdateExes        = []
    , ideUpdateDocs        = False
    , ideUpdateLicenses    = []
    , ideUpdateRtsOpts     = Nothing
    }

  a `mappend` b = IdeSessionUpdate {
      -- File and builds commands are executed in order
      ideUpdateFileCmds = ideUpdateFileCmds a ++ ideUpdateFileCmds b
    , ideUpdateExes     = ideUpdateExes     a ++ ideUpdateExes     b
    , ideUpdateLicenses = ideUpdateLicenses a ++ ideUpdateLicenses b

      -- For boolean options we just take logical disjunction
    , ideUpdateDeleteFiles = ideUpdateDeleteFiles a || ideUpdateDeleteFiles b
    , ideUpdateDocs        = ideUpdateDocs        a || ideUpdateDocs        b

      -- Everything else is right biased (see comments for IdeSessionUpdate)
    , ideUpdateGhcOpts     = ideUpdateGhcOpts     b `mplus` ideUpdateGhcOpts     a
    , ideUpdateRelIncls    = ideUpdateRelIncls    b `mplus` ideUpdateRelIncls    a
    , ideUpdateCodeGen     = ideUpdateCodeGen     b `mplus` ideUpdateCodeGen     a
    , ideUpdateEnv         = ideUpdateEnv         b `mplus` ideUpdateEnv         a
    , ideUpdateArgs        = ideUpdateArgs        b `mplus` ideUpdateArgs        a
    , ideUpdateStdoutMode  = ideUpdateStdoutMode  b `mplus` ideUpdateStdoutMode  a
    , ideUpdateStderrMode  = ideUpdateStderrMode  b `mplus` ideUpdateStderrMode  a
    , ideUpdateTargets     = ideUpdateTargets     b `mplus` ideUpdateTargets     a
    , ideUpdateRtsOpts     = ideUpdateRtsOpts     b `mplus` ideUpdateRtsOpts     a
    }

data FileInfo = FileInfo {
    fileInfoRemoteDir  :: IdeStaticInfo -> FilePath
  , fileInfoRemoteFile :: FilePath
  , fileInfoAccessor   :: Accessor ManagedFilesInternal [ManagedFile]
  }

instance Show FileInfo where
  show (FileInfo{..}) = "<<FileInfo " ++ fileInfoRemoteFile ++ ">>"

data FileCmd =
    -- | Write a file from a bytestring
    FileWrite FileInfo BSL.ByteString

    -- | Copy a local file (the FilePath is interpreted as an absolute path)
  | FileCopy FileInfo FilePath

    -- | Delete a file
  | FileDelete FileInfo
  deriving Show

{-------------------------------------------------------------------------------
  Individual updates
-------------------------------------------------------------------------------}

-- | Delete all files currently managed in this session
updateDeleteManagedFiles :: IdeSessionUpdate
updateDeleteManagedFiles = mempty { ideUpdateDeleteFiles = True }

-- | A session update that changes a source file by providing some contents.
-- This can be used to add a new module or update an existing one.
-- The @FilePath@ argument determines the directory
-- and file where the module is located within the project.
-- In case of Haskell source files, the actual internal
-- compiler module name, such as the one given by the
-- @getLoadedModules@ query, comes from within @module ... end@.
-- Usually the two names are equal, but they needn't be.
updateSourceFile :: FilePath -> BSL.ByteString -> IdeSessionUpdate
updateSourceFile fp bs = mempty { ideUpdateFileCmds = [FileWrite fileInfo bs] }
  where
    fileInfo = FileInfo {
        fileInfoRemoteFile = fp
      , fileInfoRemoteDir  = ideSourceDir
      , fileInfoAccessor   = managedSource
      }

-- | Like 'updateSourceFile' except that instead of passing the source by
-- value, it's given by reference to an existing file, which will be copied.
updateSourceFileFromFile :: FilePath -> IdeSessionUpdate
updateSourceFileFromFile fp = mempty { ideUpdateFileCmds = [FileCopy fileInfo fp] }
  where
    fileInfo = FileInfo {
        fileInfoRemoteFile = fp
      , fileInfoRemoteDir  = ideSourceDir
      , fileInfoAccessor   = managedSource
      }

-- | A session update that deletes an existing source file.
updateSourceFileDelete :: FilePath -> IdeSessionUpdate
updateSourceFileDelete fp = mempty { ideUpdateFileCmds = [FileDelete fileInfo] }
  where
    fileInfo = FileInfo {
        fileInfoRemoteFile = fp
      , fileInfoRemoteDir  = ideSourceDir
      , fileInfoAccessor   = managedSource
      }

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
updateDataFile :: FilePath -> BSL.ByteString -> IdeSessionUpdate
updateDataFile fp bs = mempty { ideUpdateFileCmds = [FileWrite fileInfo bs] }
  where
    fileInfo = FileInfo {
        fileInfoRemoteFile = fp
      , fileInfoRemoteDir  = ideDataDir
      , fileInfoAccessor   = managedData
      }

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file (the second argument),
-- which will be copied.
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate
updateDataFileFromFile remoteFile localFile = mempty {
    ideUpdateFileCmds = [FileCopy fileInfo localFile]
  }
  where
    fileInfo = FileInfo {
        fileInfoRemoteFile = remoteFile
      , fileInfoRemoteDir  = ideDataDir
      , fileInfoAccessor   = managedData
      }

-- | Deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate
updateDataFileDelete fp = mempty { ideUpdateFileCmds = [FileDelete fileInfo] }
  where
    fileInfo = FileInfo {
        fileInfoRemoteFile = fp
      , fileInfoRemoteDir  = ideDataDir
      , fileInfoAccessor   = managedData
      }

-- | Set ghc options
--
-- This function is stateless: the set of actions options is the set provided
-- by the last call to updateGhcOptions.
updateGhcOpts :: [String] -> IdeSessionUpdate
updateGhcOpts opts = mempty { ideUpdateGhcOpts = Just opts }

-- | Set RTS options for the ghc session (this does not affect executables)
--
-- This will cause a session restart.
--
-- NOTE: Limiting stack size does not seem to work for ghc 7.4
-- (https://github.com/fpco/ide-backend/issues/258).
updateRtsOpts :: [String] -> IdeSessionUpdate
updateRtsOpts opts = mempty { ideUpdateRtsOpts = Just opts }

-- | Set include paths (equivalent of GHC's @-i@ parameter).
-- In general, this requires session restart,
-- because GHC doesn't revise module dependencies when targets
-- or include paths change, but only when files change.
--
-- This function is stateless: semantically, the set of currently active
-- include paths are those set in the last call to updateRelativeIncludes.
-- Any paths set earlier (including those from 'configRelativeIncludes')
-- are wiped out and overwritten in each call to updateRelativeIncludes.
updateRelativeIncludes :: [FilePath] -> IdeSessionUpdate
updateRelativeIncludes relIncl = mempty { ideUpdateRelIncls = Just relIncl }

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt'.
updateCodeGeneration :: Bool -> IdeSessionUpdate
updateCodeGeneration b = mempty { ideUpdateCodeGen = Just b }

-- | Set environment variables
--
-- Use @updateEnv [(var, Nothing)]@ to unset @var@.
--
-- Note that this is intended to be stateless:
--
-- > updateEnv []
--
-- will reset the environment to the server's original environment.
updateEnv :: [(String, Maybe String)] -> IdeSessionUpdate
updateEnv overrides = mempty { ideUpdateEnv = Just overrides }

-- | Set command line arguments for snippets
-- (i.e., the expected value of `getArgs`)
updateArgs :: [String] -> IdeSessionUpdate
updateArgs args = mempty { ideUpdateArgs = Just args }

-- | Set buffering mode for snippets' stdout
updateStdoutBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStdoutBufferMode m = mempty { ideUpdateStdoutMode = Just m }

-- | Set buffering mode for snippets' stderr
updateStderrBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStderrBufferMode m = mempty { ideUpdateStderrMode = Just m }

-- | Set compilation targets. In general, this requires session restart,
-- because GHC doesn't revise module dependencies when targets
-- or include paths change, but only when files change.
updateTargets :: Public.Targets -> IdeSessionUpdate
updateTargets targets = mempty { ideUpdateTargets = Just targets }

-- | Build an exe from sources added previously via the ide-backend
-- updateSourceFile* mechanism. The modules that contains the @main@ code are
-- indicated in second argument to @buildExe@. The function can be called
-- multiple times with different arguments. Additional GHC options,
-- applied only when building executables, are supplied in the first argument.
--
-- We assume any indicated module is already successfully processed by GHC API
-- in a compilation mode that makes @computedImports@ available (but no code
-- needs to be generated). The environment (package dependencies, ghc options,
-- preprocessor program options, etc.) for building the exe is the same as when
-- previously compiling the code via GHC API. The module does not have to be
-- called @Main@, but we assume the main function is always @main@ (we don't
-- check this and related conditions, but GHC does when eventually called to
-- build the exe).
--
-- The executable files are placed in the filesystem inside the @build@
-- subdirectory of 'Query.getDistDir', in subdirectories corresponding
-- to the given module names. The build directory does not overlap
-- with any of the other used directories and with its path.
--
-- Logs from the building process are saved in files
-- @build\/ide-backend-exe.stdout@ and @build\/ide-backend-exe.stderr@
-- in the 'Query.getDistDir' directory.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
-- Also, after session restart, one has to call @updateSession@ at least once
-- (even with empty updates list) before calling it for @buildExe@.
-- This ensures the code is compiled again and the results made accessible.
buildExe :: [String] -> [(ModuleName, FilePath)] -> IdeSessionUpdate
buildExe extraOpts ms = mempty { ideUpdateExes = [(extraOpts, ms)] }

-- | Build haddock documentation from sources added previously via
-- the ide-backend updateSourceFile* mechanism. Similarly to 'buildExe',
-- it needs the project modules to be already loaded within the session
-- and the generated docs can be found in the @doc@ subdirectory
-- of 'Query.getDistDir'.
--
-- Logs from the documentation building process are saved in files
-- @doc\/ide-backend-doc.stdout@ and @doc\/ide-backend-doc.stderr@
-- in the 'Query.getDistDir' directory.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
buildDoc :: IdeSessionUpdate
buildDoc = mempty { ideUpdateDocs = True }

-- | Build a file containing licenses of all used packages.
-- Similarly to 'buildExe', the function needs the project modules to be
-- already loaded within the session. The concatenated licenses can be found
-- in file @licenses.txt@ inside the 'Query.getDistDir' directory.
--
-- The function expects .cabal files of all used packages,
-- except those mentioned in 'configLicenseExc',
-- to be gathered in the directory given as the first argument
-- (which needs to be an absolute path or a path relative to the data dir).
-- The code then expects to find those packages installed and their
-- license files in the usual place that Cabal puts them
-- (or the in-place packages should be correctly embedded in the GHC tree).
--
-- We guess the installed locations of the license files on the basis
-- of the haddock interfaces path. If the default setting does not work
-- properly, the haddock interfaces path should be set manually. E.g.,
-- @cabal configure --docdir=the_same_path --htmldir=the_same_path@
-- affects the haddock interfaces path (because it is by default based
-- on htmldir) and is reported to work for some values of @the_same_path@.
--
-- Logs from the license search and catenation process are saved in files
-- @licenses.stdout@ and @licenses.stderr@
-- in the 'Query.getDistDir' directory.
--
-- Note: currently 'configGenerateModInfo' needs to be set
-- for this function to work (see #86).
--
-- Note: if the executable uses TH and its module is named @Main@
-- (and so it's not compiled as a part of a temporary library)
-- 'Config.configDynLink' needs to be set. See #162.
buildLicenses :: FilePath -> IdeSessionUpdate
buildLicenses cabalsDir = mempty { ideUpdateLicenses = [cabalsDir] }
