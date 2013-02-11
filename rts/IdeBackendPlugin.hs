{-# LANGUAGE CPP #-}
module IdeBackendPlugin (plugin) where

import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return (CoreDoPluginPass "IdeBackend" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
    dynFlags <- getDynFlags
    bindsOnlyPass (mapM $ printBind dynFlags) guts
  where 
    printBind :: DynFlags -> CoreBind -> CoreM CoreBind
    printBind _dynFlags bndr@(NonRec b _) = do
#if __GLASGOW_HASKELL__ >= 706
      liftIO $ appendFile "/tmp/ghc.log" $ showSDoc _dynFlags (ppr b)
#else
      liftIO $ appendFile "/tmp/ghc.log" $ showSDoc (ppr b)
#endif
      return bndr 
    printBind _dynFlags bndr = return bndr
