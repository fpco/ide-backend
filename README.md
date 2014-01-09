ide-backend
===========

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


Changelog
---------

 *  Version 0.8. 
 
    This is a major new release with a lot of new functionality, bug fixes, and
    some minor API changes. 

     * New functionality: support for GHC 7.8, and make sure that a single
       ide-backend client library, compiled with a stock ghc, can talk to
       multiple ide-backend-servers (one for ghc 7.4, one for ghc 7.8) (#137,
       #147, #148, #149, #150, #151, #158, ghc #8006, ghc #8067, ghc Hooks
       proposal).

       NOTE: This requires new versions of both ghc 7.4 and ghc 7.8. 

       - For ghc 7.4: commit 8c021d1 (branch ide-backend-experimental-74).
         Differences from the official release: backported Hooks; backported
         fixes to #1381, #7040, #7231, #7478, #8006, #8333 (ide-backend issues
         #145 and #161). Also applied patch for #4900, although that patch is
         not accepted in HEAD yet (necessary for #118).

       - For ghc 7.8: commit e0f0172 (branch ide-backend-experimental-78).
         Since there is no official release of ghc yet, this picks a
         semi-random snapshot of the ghc tree (a93f857). Our branch differs
         from this snapshot by only a single patch (for #118/ghc #4900); we
         have made various other patches, but they have all been included in
         the official tree.

       Note that we now use ghc's standard non-release version numbering
       (7.4.2.<date> and 7.7.<date>). ide-backend supports a higher level query
       (getGhcVersion :: Query GhcVersion) where

           data GhcVersion = GHC742 | GHC78

     * New functionality: debugging API (#131).

       Breakpoints can be set/cleared using

           setBreakpoint :: IdeSession
                         -> ModuleName
                         -> Public.SourceSpan
                         -> Bool
                         -> IO (Maybe Bool)

       The existing API for binding subexpressions can be used to construct
       SourceSpans.

       When a breakpoint is set, snippets can stop with a new RunResult

           data RunResult =
             ...
             | RunBreak

       They can be resumed using

           resume :: IdeSession -> IO (RunActions Public.RunResult)

       Information about the current breakpoint (if any) can be got using

           getBreakInfo :: Query (Maybe BreakInfo)

           data BreakInfo = BreakInfo {
               breakInfoModule      :: Public.ModuleName
             , breakInfoSpan        :: SourceSpan
             , breakInfoResultType  :: Public.Type
             , breakInfoVariableEnv :: Public.VariableEnv
             }

           type VariableEnv = [(Name, Type, Value)]

       Variables can be printed and/or forced using

           printVar :: IdeSession
                    -> Public.Name
                    -> Bool
                    -> Bool
                    -> IO Public.VariableEnv

       The two booleans indicate whether new variables should be bound (cf.
       ghci's :print vs :sprint) and whether the value should be forced (:print
       vs :force). This is only valid during an active breakpoint.

       Not all of ghci's debugging funtionality is yet supported; this is
       documented in more detail in the ticket (#131, also #136). Note also
       that the API may not be entirely stable yet. In particular, having
       printVar independent of RunActions is unsatisfactory; fixing this _may_
       change the runStmt API too. This is documented in great detail in a new
       ticket (#153).

     * New functionality: generate Hoogle/Haddock (#70)

     * New functionality: distinguish between KindError and KindServerDied, and
       hide the "internal exception" when showing external exceptions (#135)

     * New functionality: a new field (configWarnings :: GhcWarnings) in the
       session config makes it possible to enable/disable certain warnings in
       a ghc version-independent manner. 

     * New functionality: allow to set compilation targets (#152) 
       
           updateTargets :: Maybe [FilePath] -> IdeSessionUpdate ()

       As part of this also added a new field called configRelativeIncludes to
       SessionConfig (#156). To see why this is necessary, consider "module A"
       in "foo/bar/A.hs" and "module B" in "foo/bar/B.hs", where module B
       imports module A, and we specify "foo/bar/B.hs" as the target. Then ghc
       will be unable to find module A because it won't know to look in
       directory "foo/bar" (before updateTargets this was not an issue because
       we specified all modules paths explicitly to ghc). Now
       configRelativeIncludes can be used to tell ghc where to look (in this
       case, it should be set to ["foo/bar"]). 
  
       Note that buildExe and co do not yet take these targets into account
       (#154).

       Also, fixed bug where we would not pass -i to the ghc server on session
       restart (this bug was mostly invisible when we were providing all source
       files explicitly).

     * New functionality: support for boot files (#155, #157).

     * New functionality: support C files, both through the API (where
       ide-backend-server compiles the .c files into .o files and dynamically
       loads/unloads these object files) and in executable generation (#122).

     * New functionality: specify additional arguments for ghc when building
       executables (#159).

     * Bugfix: Make sure ID info is updated on code changes (fixed a caching
       problem) (#142)

     * Bugfix: Make sure that updating static files triggers recompilation
       (#118). This is fixed by means of a ghc patch (in both 7.4 and 7.8; ghc
       issue #4900).
     
     * Bugfix: Avoid confusing error message in buildExe for code with type
       errors (#145, #160). 

     * Better way to deal with async exceptions in snippet interrupts in ghc
       (#58, ghc issue #8006).  The new solution works in both 7.4 and 7.8, and
       in both "regular" execution of snippets and in execution with
       breakpoints enabled/resumed execution (#133). The new approach is now in
       the official GHC tree. 

     * Upgraded to Cabal 1.18.1.2 (necessary to support GHC 7.8), and set
       things up so that it generates the necessary dynlibs when using 7.8.
       Although Cabal 1.18 reports from problems slightly differently to Cabal
       1.16; ide-backend attempts to hide this difference (#146).

     * Minor API changes:

        - RunActions now has kind * -> * ; what was previously just "RunActions"
          is now (from the client's perspective) "RunActions RunResult"
        - String argument to RunOk has been removed.
        - IdeSessionUpdate now has kind * -> * ; what was previously just
          "IdeSessionUpdate" is now "IdeSessionUpdate ()". IdeSessionUpdate
          has been given a Monad instance.
        - updateModule* is now called updateSourceFile* (because it is used for
          more than just Haskell modules).

     * Merged pull requests
       
        - Use binary-ide-backend throughout (#144)

     * Test-suite modificatons: 

        - Test for #134: multiple changes to TH dependent files in one second 
        - Test for #32: Internal paths in error messages 
        - Test for #50: Async problem with snippet interrupt
        - Test for fpco/fpco/#3043: constraints in monotypes
        - Fixed non-deterministic failure of test for #58 (#143)
        - Updated tests to support for 7.4 and 7.8 (mark allowable differences,
          use alpha-equivalence rather than syntactic identity to compare
          types, etc.), and be less picky about package versions

     * Isolated a bug in sqlite which was causing #104.  

     * Known issue: tracked #125 down to a bug in the GHC RTS, and reported
       this (https://ghc.haskell.org/trac/ghc/ticket/8648). There is no fix yet
       though. 
     
     * Known issue: updateGhcOptions has some problems; detailed in #115.

 *  Version 0.7.1

     * New functionality: types of subexpressions (#50). Known issues:

       - We sometimes report multiple types for the same span (see comments
         at the end of https://github.com/fpco/ide-backend/issues/50).

     * New functionality: report use sites of identifiers (#129)

     * New functionality: Generate .cabal files (#127)

       - Note that library data files are not installed. This should be easy
         to add if/when that is needed. Just let us know.

     * Efficiency of construction of type information (and especially
       autocompletion information) has been improved (#132).

     * Move license catenation (#72) documentation to haddocks,
       flesh it out, make code more readable, improve error output,
       fix a recently broken test

     * Add configLicenseFixed to hard-code packages with hidden
       .cabal files (#72)

     * Optimize license catenation and add some benchmark tests (#72)

     * Bugfix: for ghc progress messages in projects with 10 or more modules
       the first 9 progress message were not parsed correctly because we didn't
       allow for whitespace in between '[' and the first number ("[ 1 of 10]").

     * Merged pull requests:

        - Load CoreLicenses.txt via Template Haskell (#128)
        - Less license failures (#126)
        - Use System.IO.UTF8 to avoid character encoding issues (#117)

 *  Version 0.7.0.2

     * Use System.IO.UTF8 to avoid character encoding issues (fpco/fpco#2232)

     * Removing non-existent files should not throw an exception

     * Relaxed bounds on process, directory, tagged, and use crypto-api 0.12
       and aeson 0.6.2

     * Use binary 0.7 (as binary-ide-backend); improved Binary instance for
       Text that avoids lots of small chunks, and use the incremental API
       from binary 0.7 to avoid the use of lazy I/O

     * Clear the build directory in buildExe (temporary fix for #119)

     * Avoid stack overflow in Binary instance for lazy bytestrings (#121)

 *  Version 0.7.0.1

     * Bugfix: make sure restartSession passes the configPackageDBStack
       to ghc (#114)

 *  Version 0.7.0.0

    NOTE. This release includes a number of small API changes and behavioural
    changes since 0.6.0.2.

     * Report server crashes as SourceErrors, and implicitly restart the session
       on the _next_ call to updateSession (#107). Note that if the server is
       in dead state (i.e., after the serve crash and before the next call to
       updateSession) a call to runStmt will throw an exception (just like when
       you call runStmt when no code is compiled at all). It is the
       responsibility of the client code to check for source errors
       (using getSourceErrors) before calling runStmt.

     * It is now possible to reuse previously generated cabal macros, in order
       to improve performance (#109). The type of initSession has changed to

           initSession :: SessionInitParams -> SessionConfig -> IO IdeSession

       where

           data SessionInitParams = SessionInitParams {
               sessionInitCabalMacros :: Maybe BSL.ByteString
             }

       Similarly, restartSession now has type (#113)

           restartSession :: IdeSession -> Maybe SessionInitParams -> IO ()

       When passed Nothing it will leave the cabal macros unchanged, but when
       passed a SessionInitParams it will regenerate them or use a previously
       regenerated cabal macros file.

       The generated cabal macros can be accessed using

           getCabalMacros :: Query BSL.ByteString

     * Extended the Progress data type (#112) to contain the number of steps as
       well as a progress message. See the comments in the ticket and in the
       Haddock for Progress for some limitations, however (briefly, during
       compilation, we get non-contiguous and possibly out of order progress
       updates from ghc: [4/13], [8/13], done).

     * It is now possible to specify additional program search directories in
       *addition* to the normal `$PATH` when creating a session (#99).

           data SessionConfig = SessionConfig {
               configExtraPathDirs :: [FilePath]
               ...

       This allows different sessions within the same process (that share the
       same `$PATH` env var) to have different program search paths. In
       particular this can be used to select the instance of ghc by controlling
       which ghc binary is found.

       Note that the `configExtraPathDirs` are in addition to the `$PATH`, they
       are searched after the `$PATH`.

 *  Version 0.6.0.2

     * Fix problem with cabal_macros in package databases with multiple
       versions of the same package.

     * Merge pull request from fpco/looger (#105)

     * Traverse class default methods for id info (#106)

 *  Version 0.6.0.1

     * Now uses a private Cabal fork as a sub-repo. Branch is ide-backend.
       The package is renamed as Cabal-ide-backend. You will need to install
       this and then ghc-pkg hide it or it will cause problems (e.g. ambigious
       modules when building Setup.hs files).

     * Fix for ticket #103. The macro generation should now be faster.
       No interface change for this.

 *  Version 0.6

     * baseline version for new version + release protocol
