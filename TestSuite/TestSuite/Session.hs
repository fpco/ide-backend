-- | Working with IDE sessions
module TestSuite.Session (
    updateSessionD
  , loadModule
  , loadModulesFrom
  , loadModulesFrom'
  , getModules
  , getModulesFrom
  ) where

import Prelude hiding (mod)
import Control.Monad
import Data.IORef
import Data.List (isPrefixOf)
import Data.Monoid
import System.FilePath
import System.FilePath.Find (always, extension, find)
import Test.HUnit
import qualified Data.ByteString.Lazy.UTF8 as L

import IdeSession

updateSessionD :: IdeSession -> IdeSessionUpdate () -> Int -> IO ()
updateSessionD session update numProgressUpdates = do
  progressRef <- newIORef []

  -- We just collect the progress messages first, and verify them afterwards
  updateSession session update $ \p -> do
    progressUpdates <- readIORef progressRef
    writeIORef progressRef $ progressUpdates ++ [p]

  -- These progress messages are often something like
  --
  -- [18 of 27] Compiling IdeSession.Types.Private ( IdeSession/Types/Private.hs, dist/build/IdeSession/Types/Private.o )
  -- [19 of 27] Compiling IdeSession.GHC.API ( IdeSession/GHC/API.hs, dist/build/IdeSession/GHC/API.o )
  -- [20 of 27] Compiling IdeSession.GHC.Client ( IdeSession/GHC/Client.hs, dist/build/IdeSession/GHC/Client.p_o )
  -- [21 of 27] Compiling IdeSession.Types.Translation ( IdeSession/Types/Translation.hs, dist/build/IdeSession/Types/Translation.p_o )
  -- [23 of 27] Compiling IdeSession.State ( IdeSession/State.hs, dist/build/IdeSession/State.p_o )
  --
  -- So these numbers don't need to start at 1, may be discontiguous, out of
  -- order, and may not end with [X of X]. The only thing we can check here is
  -- that we get at most the number of progress messages we expect.
  progressUpdates <- readIORef progressRef
  assertBool ("We expected " ++ show numProgressUpdates ++ " progress messages, but got " ++ show progressUpdates)
             (length progressUpdates <= numProgressUpdates)

loadModule :: FilePath -> String -> IdeSessionUpdate ()
loadModule file contents =
    let mod =  "module " ++ mname file ++ " where\n" ++ contents
    in updateSourceFile file (L.fromString mod)
  where
    -- This is a hack: construct a module name from a filename
    mname :: FilePath -> String
    mname path = case "test/" `substr` path of
      Just rest -> dotToSlash . dropExtension . dropFirstPathComponent $ rest
      Nothing   -> takeBaseName path

    dropFirstPathComponent :: FilePath -> FilePath
    dropFirstPathComponent = tail . dropWhile (/= '/')

    dotToSlash :: String -> String
    dotToSlash = map $ \c -> if c == '/' then '.' else c

    -- | Specification:
    --
    -- > bs `substr` (as ++ bs ++ cs) == Just cs
    -- > bs `substr` _                == Nothing
    substr :: Eq a => [a] -> [a] -> Maybe [a]
    substr needle haystack
      | needle `isPrefixOf` haystack = Just $ drop (length needle) haystack
      | otherwise = case haystack of
                      []              -> Nothing
                      (_ : haystack') -> substr needle haystack'

loadModulesFrom :: IdeSession -> FilePath -> IO ()
loadModulesFrom session originalSourcesDir =
  loadModulesFrom' session originalSourcesDir $ TargetsExclude []

loadModulesFrom' :: IdeSession -> FilePath -> Targets -> IO ()
loadModulesFrom' session originalSourcesDir targets = do
  (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
  updateSessionD session (originalUpdate <> updateTargets targets) (length lm)

getModules :: IdeSession -> IO (IdeSessionUpdate (), [FilePath])
getModules session = do
  sourcesDir <- getSourcesDir session
  getModulesFrom session sourcesDir

-- | Update the session with all modules of the given directory.
getModulesFrom :: IdeSession -> FilePath -> IO (IdeSessionUpdate (), [FilePath])
getModulesFrom _session originalSourcesDir = do
  -- Send the source files from 'originalSourcesDir' to 'configSourcesDir'
  -- using the IdeSession's update mechanism.
  originalFiles <- find always
                        ((`elem` sourceExtensions) `liftM` extension)
                        originalSourcesDir
  let originalUpdate = updateCodeGeneration False
                    <> (mconcat $ map updateSourceFileFromFile originalFiles)
  return (originalUpdate, originalFiles)
