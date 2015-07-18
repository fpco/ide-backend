ide-backend
===========

[![Build Status](https://travis-ci.org/fpco/ide-backend.svg?branch=master)](https://travis-ci.org/fpco/ide-backend)

In order to use this package, you must have the `ide-backend`, `ide-backend-server`, and `ide-backend-rts` packages installed.

Hello World
-----------

The following simple example demonstrates basic usage of ide-backend.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as S8
import           Data.Monoid           ((<>))
import           IdeSession

main :: IO ()
main = do
    -- Initialization and providing some code
    sess <- initSession defaultSessionInitParams defaultSessionConfig
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

Sample output for this run:

```
[1 of 1] Compiling Main
SourceError {errorKind = KindWarning, errorSpan = Main.hs@1:1-1:30, errorMsg = "Top-level binding with no type signature: main :: IO ()"}
Hello World
Run result: RunOk
[(Main.hs@1:8-1:16,"String -> IO ()"),(Main.hs@1:8-1:30,"IO ()")]
[putStr (VarName) defined in base-4.8.0.0:System.IO at <no location info> (home base-4.8.0.0:System.IO) (imported from base-4.8.0.0:Prelude at Main.hs@1:1-1:1),putStrLn (VarName) :: String -> IO () defined in base-4.8.0.0:System.IO at <no location info> (home base-4.8.0.0:System.IO) (imported from base-4.8.0.0:Prelude at Main.hs@1:1-1:1)]
```

Versions and releases
---------------------

The ide-backend component now uses versioned releases (following the normal
package version policy). While we do not make tarballs, we will always tag
versions.

Please *only* use tagged versions and not intermediate git hashes. Instead,
please request new tagged releases (either from head or based on old versions).

Please consult the changelog below when integrating a new version of
ide-backend. The changelog is the place where we will point out:

 * new features;
 * interface changes;
 * and other relevant information such as which areas may need particular
   attention and testing during integration.
