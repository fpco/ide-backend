{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple in-prcess execution of GHC API calls, for testing, to easily
-- see stdout, stderr, exception, etc.. Uses "GhcRun" directly, in the same
-- process, without the RPC layer.
module Main where

import System.Environment
import qualified Data.List as List

import Common
import GhcRun
import Control.Concurrent
  ( forkIO
  , myThreadId
  )
import Control.Concurrent.MVar
  ( newMVar
  , putMVar
  , modifyMVar_
  , takeMVar
  , isEmptyMVar
  )
import RpcServer
import Common
import GhcRun
import Progress
import qualified Control.Exception as Ex
import Control.Monad (void)

-- A program, for debugging and experiments, that type-checks
-- a file and runs its main function.
-- It interfaces with the GHC API code in-process, as opposed
-- to out-of-process that the IdeSession API uses.
type PCounter = Int
data GhcRequest  =
  ReqCompute (Maybe [String]) FilePath Bool (Maybe (String, String))
  deriving Show
data GhcResponse = RespWorking PCounter | RespDone RunOutcome
  deriving Show

defOpts :: [String]
defOpts = [ "-hide-all-packages"
          , "-package parallel", "-package base", "-package old-time" ]

main :: IO ()
main = do
  args <- getArgs
  target <-
    case args of
      [f] -> return f
      []  -> return "test/MainModule/ParFib.hs"
      _   -> fail "usage: in-process [file]"

  mvCounter <- newMVar (Right 0)  -- Report progress step [0/n], too.
  let forkCatch :: IO () -> IO ()
      forkCatch p = do
        tid <- myThreadId
        void $ forkIO $ Ex.catch p (\ (ex :: Ex.SomeException) ->
                                     Ex.throwTo tid ex)
  putStrLn ""

  forkCatch $ do
    let incrementCounter (Right c) = Right (c + 1)
        incrementCounter (Left _)  = error "ghcServerEngine: unexpected Left"
        updateCounter = do
          -- Don't block, GHC should not be slowed down.
          b <- isEmptyMVar mvCounter
          if b
            -- Indicate that another one file was type-checked.
            then putMVar mvCounter (Right 1)
            -- If not consumed, increment count and keep working.
            else modifyMVar_ mvCounter (return . incrementCounter)
        handlerOutput _ = updateCounter
        handlerRemaining _ = return ()  -- TODO: put into logs somewhere?
    runOutcome <- checkModule [target] (optsToDynFlags defOpts)
                                   True Nothing 3
                                   handlerOutput handlerRemaining
    b <- isEmptyMVar mvCounter
    if b
      then putMVar mvCounter (Left runOutcome)
      else modifyMVar_ mvCounter (return . const (Left runOutcome))

  let p :: Int -> Progress GhcResponse GhcResponse
      p counter = Progress $ do
        -- Block until GHC processes the next file.
        merrs <- takeMVar mvCounter
        case merrs of
          Right new -> do
            -- Add the count of files type-checked since last time reported.
            -- The count is 1, unless the machine is busy and @p@ runs rarely.
            let newCounter = new + counter
            return $ Right (RespWorking newCounter, p newCounter)
          Left errs ->
            return $ Left $ RespDone errs
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStr (show n)
      f (RespWorking c)         = c  -- advancement counter
      f (RespDone _)            = error "updateSession: unexpected RespDone"
      g (RespWorking _)         = error "updateSession: unexpected RespWorking"
      g (RespDone r)            = r
      pro =  bimapProgress f g $ p 0
  (errs, resOrEx) <- progressWaitConsume displayCounter pro
  putStrLn $ "\nErrors and warnings:\n"
             ++ List.intercalate "\n" (map formatSourceError errs)
             ++ "\n"
  putStrLn $ "Run result: "
             ++ case resOrEx of
                  Just (Left ident) -> ident
                  Just (Right ex)   -> ex
                  Nothing           -> "Run failed."
             ++ "\n"
