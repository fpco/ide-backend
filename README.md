# ide-backend

[![Build Status](https://travis-ci.org/fpco/ide-backend.svg?branch=master)](https://travis-ci.org/fpco/ide-backend)

ide-backend drives the GHC API to build, query, and run your code.  It
handles the tricky bits of driving the GHC API for use with integrated
development environments.  It was originally developed for the School
of Haskell and FP Haskell Center, and is now an open source project.
With [stack-ide](https://github.com/commercialhaskell/stack-ide), IDEs
and text editors can talk to ide-backend with a JSON protocol.

## Installation

[stack](https://github.com/commercialhaskell/stack) is required to
build ide-backend.  Once you have stack, run `stack install`.

## Running the tests

To run the tests, just do `stack test`.  This won't run a complete
test, as it will only use your current GHC version, but that should be
sufficient for most development.  We can leave it to
[travis](https://travis-ci.org/fpco/ide-backend) to try multiple GHC
versions.

## Hello World

The following simple example demonstrates basic usage of ide-backend.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as S8
import           Data.Monoid           ((<>))
import           IdeSession

main :: IO ()
main = do
    -- Initialization and providing some code
    config <- sessionConfigFromEnv
    sess <- initSession defaultSessionInitParams config
        { configLocalWorkingDir = Nothing }
    let upd = updateSourceFile "Main.hs" "main = putStrLn \"Hello World\""
           <> updateCodeGeneration True
           <> updateGhcOpts ["-Wall"]
    updateSession sess upd print -- print is used for progress updates

    -- Print errors and warnings
    errs <- getSourceErrors sess
    mapM_ print errs

    -- Run the code
    ra <- runStmt sess "Main" "main"
    let loop = do
            res <- runWait ra
            case res of
                Left bs -> S8.putStr bs >> loop
                Right rr -> putStrLn $ "Run result: " ++ show rr
    loop

    -- Get some type information
    expTypes <- getExpTypes sess
    print $ expTypes "Main" SourceSpan
        { spanFilePath = "Main.hs"
        , spanFromLine = 1
        , spanFromColumn = 8
        , spanToLine = 1
        , spanToColumn = 9
        }

    -- Autocompletion
    autoCompletion <- getAutocompletion sess
    print $ autoCompletion "Main" "putS"
```

To run this, use `stack runghc example.hs`. The output should look
something like:

```
[1 of 1] Compiling Main
SourceError {errorKind = KindWarning, errorSpan = Main.hs@1:1-1:30, errorMsg = "Top-level binding with no type signature: main :: IO ()"}
Hello World
Run result: RunOk
[(Main.hs@1:8-1:16,"String -> IO ()"),(Main.hs@1:8-1:30,"IO ()")]
[putStr (VarName) defined in base-4.8.0.0:System.IO at <no location info> (home base-4.8.0.0:System.IO) (imported from base-4.8.0.0:Prelude at Main.hs@1:1-1:1),putStrLn (VarName) :: String -> IO () defined in base-4.8.0.0:System.IO at <no location info> (home base-4.8.0.0:System.IO) (imported from base-4.8.0.0:Prelude at Main.hs@1:1-1:1)]
```
