{-# LANGUAGE CPP #-}
module IdeBackendPlugin (plugin, ideBackendPluginState) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return (CoreDoPluginPass "IdeBackend" pass : todo)

ideBackendPluginState :: IORef String 
{-# NOINLINE ideBackendPluginState #-}
ideBackendPluginState = unsafePerformIO $ do
  appendFile "/tmp/ghc.log" "created ide backend plugin state\n" 
  newIORef "" 

record :: String -> CoreM ()
record str = liftIO $ do
  oldState <- readIORef ideBackendPluginState
  writeIORef ideBackendPluginState (oldState ++ str)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
    dynFlags <- getDynFlags
    bindsOnlyPass (mapM $ printBind dynFlags) guts
  where 
    printBind :: DynFlags -> CoreBind -> CoreM CoreBind
    printBind _dynFlags bndr@(NonRec b _) = do
#if __GLASGOW_HASKELL__ >= 706
      record $ showSDoc _dynFlags (ppr b)
      liftIO $ appendFile "/tmp/ghc.log" $ "pass: " ++ showSDoc _dynFlags (ppr b)
#else
      record $ showSDoc (ppr b)
      liftIO $ appendFile "/tmp/ghc.log" $ "pass: " ++ showSDoc (ppr b)
#endif
      return bndr 
    printBind _dynFlags bndr = return bndr
