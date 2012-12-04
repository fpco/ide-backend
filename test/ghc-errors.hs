module Main (main) where

import System.Unix.Directory (withTemporaryDirectory)
import Data.Monoid ((<>), mempty)

--------------------------------------------------------------------------------

import Control.Monad
import System.Directory
import System.FilePath ((</>), (<.>), takeExtension)
import Data.Monoid (Monoid(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Common
import GhcServer

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

-- In this implementation, it's fully applicative, and so invalid sessions
-- can be queried at will. Note that there may be some working files
-- produced by GHC while obtaining these values. They are not captured here,
-- so queries are not allowed to read them.
data Computed = Computed
  [SourceError] -- ^ last compilation and run errors

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

-- | A session update that changes a source module. Modules can be added,
-- updated or deleted.
--
updateModule :: ModuleChange -> IdeSessionUpdate
updateModule mc = IdeSessionUpdate $ \ sess@IdeSession{ideConfig} ->
  case mc of
    ModulePut m bs -> do
      BS.writeFile (internalFile ideConfig m) bs
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
    let ideComputed = Nothing  -- can't query before the first update
        ideNewOpts  = Nothing  -- options from SessionConfig used initially
        ideGenerateCode = False
        ideConfig = sessionConfig
    ideGhcServer <- forkGhcServer ["-no-user-package-conf"]
    let sP = IdeSession{..}

    -- Test the computations.
    putStrLn "----- 1 ------"
    newSess <- originalUpdate sP

    let req = ReqCompile Nothing configSourcesDir False
    RespDone (r, Nothing) <- rpcGhcServer ideGhcServer req
    let s0 = newSess { ideComputed = Just (Computed r) }

    putStrLn "----- 2 ------"
    let req = ReqCompile Nothing configSourcesDir False
    _ <- rpcGhcServer ideGhcServer req

    putStrLn "----- 3 ------"
  where
    sessionConfig = SessionConfig{ configSourcesDir
                                 , configWorkingDir = configSourcesDir
                                 , configDataDir    = configSourcesDir
                                 , configTempDir    = "."
                                 , configStaticOpts = ["-no-user-package-conf"]
                                 }

    IdeSessionUpdate originalUpdate =
         (updateModule $ ModuleSource (ModuleName "B.hs") "test/AerrorB/B.hs")
      <> (updateModule $ ModuleSource (ModuleName "A.hs") "test/AerrorB/A.hs")

main :: IO ()
main = withTemporaryDirectory "ide-backend-test" check

