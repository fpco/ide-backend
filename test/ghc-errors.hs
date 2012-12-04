module Main (main) where

import System.Unix.Directory (withTemporaryDirectory)
import Data.Monoid ((<>), mempty)

--------------------------------------------------------------------------------

import Control.Monad
import Control.Concurrent
import System.IO (openBinaryTempFile, hClose)
import System.Directory
import System.FilePath ((</>), (<.>), splitFileName, takeExtension)
import qualified Control.Exception as Ex
import Data.Monoid (Monoid(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Common
import GhcServer
import Progress

-- | This is a state token for the current state of an IDE session. We can run
-- queries in the current state, and from this state we can perform a batch
-- of updates, ultimately leading to a new 'IdeSession'.
--
-- Note that these state tokens are not persistent, once we perform an update
-- and get a new 'IdeSession', the old one is no longer valid. (And if you do
-- accidentally use an old invalid one, then it's a dynamically checked error.)
--
data IdeSession = IdeSession
  { ideConfig    :: SessionConfig
  , ideGhcServer :: GhcServer
  , ideToken     :: StateToken
    -- The result computed by the last 'updateSession' invocation.
  , ideComputed  :: Maybe Computed
    -- Compiler dynamic options. If they are not set, the options from
    -- SessionConfig are used.
  , ideNewOpts   :: Maybe [String]
    -- Whether to generate code in addition to type-checking.
  , ideGenerateCode :: Bool
  }

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {

       -- | The directory to use for managing source files.
       configSourcesDir :: FilePath,

       -- | The directory to use for session state, such as @.hi@ files.
       configWorkingDir :: FilePath,

       -- | The directory to use for data files that may be accessed by the
       -- running program. The running program will have this as its CWD.
       configDataDir :: FilePath,

       -- | The directory to use for purely temporary files.
       configTempDir :: FilePath,

       -- | GHC static options. Can also contain default dynamic options,
       -- that are overriden via session update.
       configStaticOpts :: [String]
     }

-- This could be equally well implemented as a new mvar created per
-- each new session state, but this implementation has some minor advantages.
-- It gives more useful error messages and provides a counter of session
-- updates, which can be used, e.g.,  to roughly estimate how badly outdated
-- the info therein is, in order to decide whether to show it to the user
-- while waiting for a newer version.
data StateToken = StateToken (MVar Int) Int

initToken :: IO StateToken
initToken = do
  mv <- newMVar 0
  return $ StateToken mv 0

-- Invalidates previous sessions, returns a new token for the new session.
incrementToken :: StateToken -> IO StateToken
incrementToken token@(StateToken mv _) = do
  checkToken token
  let incrCheckToken current = do
        let newT = current + 1
        return (newT, StateToken mv newT)
  modifyMVar mv incrCheckToken

checkToken :: StateToken -> IO ()
checkToken (StateToken mv k) = do
  current <- readMVar mv
  when (k /= current)
    $ fail $ "Invalid session token " ++ show k ++ " /= " ++ show current

-- In this implementation, it's fully applicative, and so invalid sessions
-- can be queried at will. Note that there may be some working files
-- produced by GHC while obtaining these values. They are not captured here,
-- so queries are not allowed to read them.
data Computed = Computed
  [SourceError] -- ^ last compilation and run errors

ensureDirEmpty :: FilePath -> IO ()
ensureDirEmpty dir = do
  cnts <- getDirectoryContents dir
  when (any (`notElem` [".", ".."]) cnts)
    $ fail $ "Directory " ++ dir ++ " is not empty"

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession ideConfig@SessionConfig{..} = do
  ensureDirEmpty configSourcesDir
  ensureDirEmpty configWorkingDir
  ensureDirEmpty configDataDir
  let ideComputed = Nothing  -- can't query before the first update
      ideNewOpts  = Nothing  -- options from SessionConfig used initially
      ideGenerateCode = False
  ideToken <- initToken
  ideGhcServer <- forkGhcServer configStaticOpts
  return IdeSession{..}

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
--
data IdeSessionUpdate = IdeSessionUpdate (IdeSession -> IO IdeSession)

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \ sess -> return sess
  mappend (IdeSessionUpdate f) (IdeSessionUpdate g) =
    IdeSessionUpdate $ f >=> g

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so it returns a 'Progress'
-- which can be used to monitor and wait on the operation.
-- While the progress is in operation, session state tokens
-- remain valid as usual. If the progress fails or is canceled,
-- all it's observable internal state changes are rolled back
-- and another progress can be initiated. The semantics of @updateFiles@
-- and @updateSession@ is unspecified while any progress runs.
--
updateSession :: IdeSession -> IdeSessionUpdate -> IO IdeSession
updateSession sess (IdeSessionUpdate update) = do
  -- First, invalidating the current session ASAP, because the previous
  -- computed info will shortly no longer be in sync with the files.
  newToken <- incrementToken $ ideToken sess

  -- Then, updating files ASAP (using the already invalidated session).
  newSess@IdeSession{ ideConfig=SessionConfig{configSourcesDir}
                    , ideGhcServer
                    , ideNewOpts
                    , ideGenerateCode } <- update sess

  -- Last, communicating with the GHC server.
  let f (RespWorking c)         = c  -- advancement counter
      f (RespDone _)            = error "updateSession: unexpected RespDone"
      g (RespWorking _)         = error "updateSession: unexpected RespWorking"
      g (RespDone (_, Just _))  = error "updateSession: unexpected Just"
      g (RespDone (r, Nothing)) = newSess { ideToken    = newToken
                                          , ideComputed = Just (Computed r) }
      req = ReqCompile ideNewOpts configSourcesDir ideGenerateCode

  let handler :: Progress PCounter IdeSession -> IO IdeSession
      handler = progressWaitConsume (\_ -> return ())

  rpcGhcServer ideGhcServer req (handler . bimapProgress f g)

-- | Writes a file atomically.
--
-- The file is either written sucessfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  Ex.bracketOnError
    (openBinaryTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

-- | A session update that changes a source module. Modules can be added,
-- updated or deleted.
--
updateModule :: ModuleChange -> IdeSessionUpdate
updateModule mc = IdeSessionUpdate $ \ sess@IdeSession{ideConfig} ->
  case mc of
    ModulePut m bs -> do
      writeFileAtomic (internalFile ideConfig m) bs
      return sess
    ModuleSource m p -> do
      copyFile p (internalFile ideConfig m)
      return sess
    ChangeCodeGeneration b -> return $ sess {ideGenerateCode = b}

-- @OptionsSet@ affects only 'updateSession', not 'runStmt'.
data ModuleChange = ModulePut    ModuleName ByteString
                  | ModuleSource ModuleName FilePath
                  | ChangeCodeGeneration Bool

newtype ModuleName = ModuleName String
  deriving Show

internalFile :: SessionConfig -> ModuleName -> FilePath
internalFile SessionConfig{configSourcesDir} (ModuleName n) =
  let ext = takeExtension n
  in if ext `elem` cpExtentions
     then configSourcesDir </> n            -- assume full file name
     else configSourcesDir </> n <.> ".hs"  -- assume bare module name






--------------------------------------------------------------------------------

check :: FilePath -> IO ()
check configSourcesDir = do
    -- Init session.
    sP <- initSession sessionConfig
    -- Test the computations.
    putStrLn "----- 1 ------"
    s0 <- updateSession sP originalUpdate
    putStrLn "----- 2 ------"
    s2 <- updateSession s0 update1
    putStrLn "----- 3 ------"
  where
    sessionConfig = SessionConfig{ configSourcesDir
                                 , configWorkingDir = configSourcesDir
                                 , configDataDir    = configSourcesDir
                                 , configTempDir    = "."
                                 , configStaticOpts = ["-no-user-package-conf"]
                                 }

    originalUpdate = updateModule (ChangeCodeGeneration False)
                     <> (updateModule $ ModuleSource (ModuleName "B.hs") "test/AerrorB/B.hs")
                     <> (updateModule $ ModuleSource (ModuleName "A.hs") "test/AerrorB/A.hs")

    update1 = mempty

{-
    a = BS.pack $ unlines [ "module Main where"
                          , "import B"
                          , "main :: IO ()"
                          , "main = B.b"
                          ]

    b = BS.pack $ unlines [ "module B where"
                          , "b :: IO ()"
                          , "b = return ()"
                          ]
-}

main :: IO ()
main = withTemporaryDirectory "ide-backend-test" check

