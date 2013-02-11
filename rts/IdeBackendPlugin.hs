module IdeBackendPlugin (plugin) where

import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  liftIO $ appendFile "/tmp/ghc.log" "Hello!"
  return todo
