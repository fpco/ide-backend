import Distribution.Simple
import Distribution.Simple.LocalBuildInfo ( withPrograms )
import Distribution.Simple.Program ( runProgram, lookupProgram, ghcPkgProgram )
import Distribution.Simple.Setup ( fromFlag, configVerbosity, buildVerbosity )
import Distribution.Simple.Utils ( notice, die )

import Control.Exception ( bracket )
import Control.Monad ( join, when, forM_, liftM2 )
import System.Directory ( getCurrentDirectory, setCurrentDirectory
                        , removeDirectoryRecursive, removeFile
                        , doesDirectoryExist, doesFileExist
                        , getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>), takeExtension )

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


main :: IO ()
main = join $ liftM2 mainWith getArgs getCurrentDirectory

mainWith :: [String] -> FilePath -> IO ()
mainWith cmdLineArgs cwd = defaultMainWithHooksArgs hooks cmdLineArgs
  where
    hooks = simpleUserHooks {

      postConf  = \args flags pd lbi ->
        do configureRts lbi flags
           (postConf simpleUserHooks) args flags pd lbi

    , buildHook = \pd lbi hs flags ->
        do buildRts lbi flags
           makeRtsRelocatable lbi flags
           makeTarball "embedded-rts.tgz" cwd [dirToEmbed]
           (buildHook simpleUserHooks) pd lbi hs flags

    , preClean = \args flags ->
        do onRtsDir $ runSetup "clean"
           (preClean simpleUserHooks) args flags
    }


    dirToEmbed = "embedded-rts"
    pkgDbPath  = dirToEmbed </> "pkgdb"

    onRtsDir = withCurrentDirectory "ide-backend-rts"

    rawRunSetup args     = defaultMainArgs (args ++ ["--builddir=dist"])
    runSetup cmd         = rawRunSetup [cmd]
    rerunSetupWith flags = rawRunSetup (cmdLineArgs ++ flags)
      -- NB. we must set builddir explicitly (and as the final arg)
      -- since when rerunning the command during configuration, the default
      -- builddir may be changed by a flag (e.g. as "stack" does), and if we
      -- are not aware of the right builddir on the subsequent calls, we won't
      -- find the configuration info and fail.


    -- Run "Setup configure" on the directory of the rts.
    -- We ensure that we pass exactly the same command line arguments
    -- we received (since those can be indicating the version of ghc
    -- with which we are compiling the server) but override the location
    -- of the output, since we want the package-db on "embedded-rts"
    configureRts lbi flags = do
      let verbosity = fromFlag (configVerbosity flags)
      notice verbosity "configuring rts..."

      let dirToEmbedFullPath = cwd </> dirToEmbed
          pkgDbFullPath = cwd </> pkgDbPath

      outOfTheWay dirToEmbed
      runGhcPkg ["init", pkgDbPath] lbi verbosity

      onRtsDir $
        rerunSetupWith [
            "--package-db=" ++  pkgDbFullPath
          , "--libdir="     ++ (dirToEmbedFullPath </> "lib")
          , "--bindir="     ++ (dirToEmbedFullPath </> "bin")
          , "--datadir="    ++ (dirToEmbedFullPath </> "share")
          , "--docdir="     ++ (dirToEmbedFullPath </> "doc")
          , "--htmldir="    ++ (dirToEmbedFullPath </> "doc")
          , "--haddockdir=" ++ (dirToEmbedFullPath </> "doc")
          , "--enable-library-for-ghci"
          ]


    -- Builds the rts, which will end up in embedded-rts, and
    -- registers it to a package-db contained there as well
    buildRts lbi flags = do
      let verbosity = fromFlag (buildVerbosity flags)
      notice verbosity "building rts..."

      onRtsDir $ do
        runSetup "build"

      notice verbosity "locally registering rts..."
      onRtsDir $ do
        runSetup "copy"
        runSetup "register"

    -- This hack is a workaround to Cabal not having yet (as of 1.22)
    -- a clear story regarding relocatable packages. We just replace
    -- every occurrence of 'cwd </> dirToEmbed' by the string "${pkgroot}"
    -- everywhere in the installed-package-conf and run 'ghc-pkg recache'
    -- afterwards.
    makeRtsRelocatable lbi flags = do
      let verbosity = fromFlag (buildVerbosity flags)
      notice verbosity "making rts a relocatable package..."

      pkgDb_files <- getDirectoryContents pkgDbPath
      let conf_files = filter ((== ".conf").takeExtension) pkgDb_files

      -- we expect only one conf file, if we couldn't find it the hack
      -- has failed  (better to detect here than at runtime!)
      when (null conf_files) $
        die "Couldn't file a conf file to hack"

      forM_ (map (pkgDbPath </>) conf_files) $ \conf_file -> do

        -- NB. ghc-pkg expects conf files to be in utf-8
        contents <- T.decodeUtf8 `fmap` BS.readFile conf_file

        -- we do a simple text substitution instead of parsing the conf file
        -- using Distribution.InstalledPackageInfo; the text substitution is
        -- safe enough and arguably more future-proof.
        let hack = T.replace (T.pack $ cwd </> dirToEmbed) (T.pack "${pkgroot}")
            new_contents = hack contents

        -- make sure that we have indeed made a substitution, otherwise this
        -- will blow at runtime....
        when (contents == new_contents) $
          die "No substitution, hack failed"

        BS.writeFile conf_file $ T.encodeUtf8 (new_contents)

      -- Update the package cache
      runGhcPkg ["recache", "--package-db=" ++ pkgDbPath] lbi verbosity

    runGhcPkg args lbi verb =
      let Just ghc_pkg = lookupProgram ghcPkgProgram (withPrograms lbi)
      in runProgram verb ghc_pkg args


-- Available in directory-1.2.3.0
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory dir
    action

-- Removes the given file or directory
outOfTheWay :: FilePath -> IO ()
outOfTheWay fileOrDir = do
  is_dir <- doesDirectoryExist fileOrDir
  if is_dir
    then removeDirectoryRecursive fileOrDir
    else do is_file <- doesFileExist fileOrDir
            when is_file $
              removeFile fileOrDir

makeTarball :: FilePath -> FilePath -> [FilePath] -> IO ()
makeTarball tarball base contents =
  LBS.writeFile tarball . GZip.compress . Tar.write =<< Tar.pack base contents
