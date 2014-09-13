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

 *  Version 0.9

    * Required ghc patch levels:

      - For ghc 7.4: 796f4f89aeaacc778c75f7f05ecf6e37279841ce
      - For ghc 7.8: 1cb68c640a7066532eab990956b3220036e7ee31

    * This release requires a new version of the ide-backend-rts (0.1.3),
      which makes sure Handles get reset even when the snippet fails

    * New features:

      - Support for the official ghc 7.8.3 release, and moved to Cabal 1.18.1.3
        for Cabal-ide-backend.

      - buildExe now supports setting targets. The API for setting targets has
        changed as is now

            updateTargets :: Targets -> IdeSessionUpdate

        where

            data Targets = TargetsInclude [FilePath]
                         | TargetsExclude [FilePath]

      - Ability to change include paths dynamically (#178). Note that changing
        include paths (as well as changing targets) will cause a server restart.

      - Ability to hide and unhide packages dynamically, and change Safe
        Haskell flags dynamically (#185, #224). Unlike we had previously
        thought, we can in fact do this without a server restart.

      - Executables can now be run from ide-backend with the same API as
        running snippets (#181). Most of our runStmt tests in the test suite
        now have an according runExe test (which can however be disabled with
        the --no-exe to the test suite). Note that we _do_ expect _some_
        semantic differences between tests and snippets through the API; in
        particular, since we don't manage the executable, we cannot affect its
        I/O buffering modes or encoding settings. Also, the RunActions returned
        by runExe eventually return an ExitCode rather than a RunResult.

      - Snippets now run as independent processes and the "Running" state is
        gone from ide-backend. The RPC framework is modified to be able to
        deal with multiple RPC conversations (and fixed some bugs that were
        introduced in that change: #225); RPC conversations with the split off
        processes happen over named pipes. This uses 'forkProcess', for which
        we have identified and fixed a number of ghc RTS bugs (#9377, #9347,
        #9296, #9295, #9284). The use of a separate process is necessary
        because we need to be able to change process global state for the
        snippet (buffering, current working directory, value of getArgs, etc.).
        Issue #206 was partly related to these ghc bugs.  One consequence of
        this is that it is now possible to change data files while a snippet is
        running (#176).

      - The API for changing environment variables is changed (#202). It is now

            updateEnv :: [(String, Maybe String)] -> IdeSessionUpdate

        and is intended to be stateless rather than cumulative as it had been.

      - The API for setting options is changed. We no longer make a (public)
        distinction between static ghc options and dynamic ghc options, and
        the configStaticOpts are gone from the ide-backend configuration.
        Instead, we simply have

            updateGhcOpts :: [String] -> IdeSessionUpdate

        and ide-backend handles the distinction entirely internally. Note that
        so far the only option that we have come across that are truly static
        (i.e., require a session restart) are changes to the linker flags.
        Anything else (including changes to the package visibility) we can
        handle server side without a restart.

      - Similarly, removed the relative includes from the configuration and
        moved them to the session init parameters where they should have been
        from the start (because they can be changed dynamically).

        Note that you need path `""` in relative includes for `updateTargets`
        and `buildExe` to be aware of the same set of source directories
        (#184).

    * Closed issues:

      - Asymptotic performance improvement (#167, ghc #7478) and added
        performance unit tests. These are very rough at the moment and can fail
        simply due to high system load, for instance.

      - Make sure ghc sees changes to dependent files (#134, ghc #7473).

      - #169 TODO. I'm not sure I understand exactly what, if anything,
        we changed here.

      - Invoking the Cabal functions that print to stdout and stderr is now
        done via our RPC (#180) that starts a separate binary
        ide-backend-exe-cabal. This is necessary so that we can change process
        global state such as the current working directory without affecting
        the rest of the system.

        The stdout output it redirected to a log file and analyzed to provide
        Progress updates. The binary has to be present on the path. E.g., when
        you run ide-backend tests you can add the extra fpco-stock-7.4 path
        component (see SETUP.md), into which the binary is installed by default
        with 'cabal install':

         IDE_BACKEND_EXTRA_PATH_DIRS=~/env/fpco-patched-7.4/local/bin:~/env/fpco-patched-7.4/dot-cabal/bin:~/env/fpco-stock-7.4/dot-cabal/bin \
         IDE_BACKEND_PACKAGE_DB=~/env/fpco-patched-7.4/dot-ghc/snippet-db \
         dist/build/ghc-errors/ghc-errors

      - forceCancel of a snippet now sends sigKILL rather than "requesting"
        a server shutdown (#173).

      - TODO: #191 ?

      - configRelativeIncludes taken into account when building C files (#212).

      - Make it possible to override path to GHC (#205).

      - Disable idle GC in the server (#206).

      - Don't lose location information when getting multiple error messages
        (#213).

      - Make sure we don't lose output from C code by flushing C buffers as well
        as Haskell buffers (#210).

      - Make sure that we can set linker flags dynamically (#214).

      - Fix inconsistent user package database loading (#221).

      - Make sure that runStmt can safely be interrupted without putting the
        main session in an inconsistent state or leading to deadlock (#219,
        #220) and that it will terminate the server if it does get interrupted
        (#231).

      - Make sure server terminates when client does (#194).

      - Be consistent (GHC API/exe building) about Haskell2010 (#190).

      - Better fix to #119: we no longer wipe the entire build directory when
        building executables, but instead update the timestamps of .o files.
        This gives us better performance when building executables.

      - Unload object file when a previously correct C file is replaced with
        one containing errors (#201) or when the C file is removed from the
        session (#241).

      - Make sure object files are resolved correctly, even if they were
        previously loaded (#228, #230) and properly report linker errors
        to the client (#242).

      - Make sure relevant ghc options are passed to ghc when compiling C files
        (#218) and recompile C files when options are changed (#214).

      - Make sure calling Haddock does not change the source files in the
        session (#238).

      - Make sure the client gets as much information as possible on a hard
        server crash (#239).

      - Remove previous error logs on calls to buildExe etc (#245) so that
        on a call to buildExe we don't leave confusing old error messages lying
        around.

      - Make sure to generate position independent code (#244).

    * Other minor bug fixes and changes:

      - Don't skip ghc progress messages that include "[TH]" (these were
        confusing our parser)
      - Changed bounds on dependencies
      - Internal refactoring of the updateSession logic, which is now a lot
        more comprehensible (mostly possible now because we no longer have
        a "Running" state, and starting a snippet is now independent of the
        main session). This implies a minor API change; IdeSessionUpdate is no
        longer a Monad, and no longer takes a type argument. It still satisifes
        Monoid, however, and this should barely affect client code (other than
        perhaps a type annotation here and there, replacing `IdeSessionUpdate
        ()` with `IdeSessionUpdate`). This also resolves a race condition that
        was present in `updateSession` (where a concurrent update session might
        grab the session lock while the first session update was attempting to
        restart the session). Also, better treatment of exceptions during
        session updates (#250, #253).
      - Added updateDeleteManagedFiles session update to remove all managed
        (source and data) files from a session.
      - Catch and report exceptions thrown by Haddock building.

    * Test suite has been significantly refactored (#217, #237, #231), and is
      now far more modular (dramatically improved compile times), can re-use
      sessions for tests (which has brought to light quite a few bugs) as well
      as in parallel (although the latter has not been tested sufficiently
      yet), and uses tasty rather than test-framework. Various minor fixes to
      the tests:

      - GHC 7.8 started using fancy (unicode) quotes, which confused some tests.
      - Added test for .hsboot files in subdirectories (#177)
      - Don't rely on availability of profiling libs in the tests.
      - Don't rely on the IsString instance for Data.ByteString because it
        uses `pack` and thus throws away UTF8 data (#234).
      - Test for package registration fixed (#250). Note that a running session
        will NOT notice newly installed package until it is restarted (nor will
        it notice that it needs to be restarted -- this needs to be initiated
        with a call to restartSession).

    * Open issues:

      - The ghc linker is broken in the threaded runtime, which will affect
        bindings to C libraries (ghc #8648). We suspect that #175 is caused
        by this.
      - On OSX we do not detect when a snippet server dies (#229). The problem
        does not occur on Linux.
      - buildExe sometimes compiles modules more than once (#189). This should
        not change anything semantically but we have to allow for it in the
        tests (since we get more progress messages than we expect) and of course
        it has a performance impact.
      - Building executables leaves .o and .dyn_o files in the session src/
        directory (#249). The client should not really be affected by this
        however.
      - We currently _always_ restart the session when the relative includes
        or targets change. This is correct but conservative and unnecessary.
        Figuring out precisely when this is needed however is rather difficult
        and probably not worth the effort. (Note that we don't restart when
        the options "change" (by a call to updateTargets etc) to their current
        value in the session.
      - The debugging API is **disabled** in this release, as it needs to be
        updated to work with the separate server (forkProcess) for running
        snippets.

 *  Version 0.8.

    This is a major new release with a lot of new functionality, bug fixes, and
    some minor API changes.

     * New functionality: support for GHC 7.8, and make sure that a single
       ide-backend client library, compiled with a stock ghc, can talk to
       multiple ide-backend-servers (one for ghc 7.4, one for ghc 7.8)
       (#137, #147, #148, #149, #150, #151, #158, ghc #8006, ghc #8067,
       ghc Hooks proposal). Setup instructions are included in "setup/SETUP.md".

       NOTE: This requires new versions of both ghc 7.4 and ghc 7.8.

       - For ghc 7.4: commit 8c021d1 (branch ide-backend-experimental-74).
         Differences from the official release: backported Hooks; backported
         fixes to #1381, #7040, #7231, #7478, #8006, #8333
         (ide-backend issues #145 and #161). Also applied patch for #4900,
         although that patch is not accepted in HEAD yet (necessary for #118).

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

     * New functionality: generate Hoogle/Haddock (#70). Please read
       the documentation of @buildDoc@, test and suggest improvements. See also
       https://github.com/fpco/ide-backend/issues/70#issuecomment-32031570

     * New functionality: distinguish between KindError and KindServerDied, and
       hide the "internal exception" when showing external exceptions (#135)

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
       (#154), except for configRelativeIncludes, which are, in particular,
       inserted into the hs-source-dirs field of .cabal, if set.

       Also, fixed bug where we would not pass -i to the ghc server on session
       restart (this bug was mostly invisible when we were providing all source
       files explicitly).

     * New functionality: support for boot files (#155, #157).

     * New functionality: support C files (and .h files), both through
       the API (where ide-backend-server compiles the .c files
       into .o files and dynamically loads/unloads these object files)
       and in executable generation (#122).

     * New functionality: specify (as the first argument of buildExe)
       additional arguments for ghc when building executables (#159).

     * Bugfix: setting ghc options is now stateless. We still have

           configStaticOpts :: [String]

       as before, which can be used for things like package options, but changed

           updateGhcOptions :: Maybe [String] -> IdeSessionUpdate

       to

           updateDynamicOpts :: [String] -> IdeSessionUpdate

       with the following semantics: the full set of active options is those
       specified in configStaticOpts, plus those (and only those) set in the
       last call to updateDynamicOpts. In other words, setting

           updateDynamicOpts ["-Wall", "-Werror"]

       and then later

           updateDynamicOpts ["-Wall"]

       now does the right thing (#115).

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
       Although Cabal 1.18 reports problems slightly differently to Cabal
       1.16, ide-backend attempts to hide this difference (#146). There is
       still a minor problem on OSX when using an in-place compiler due to
       either a bug in ghc or a bug in cabal (#8266); see #164.

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
        - Tweaks to getDotCabal (#141)

     * Test-suite modificatons:

        - Test for #134: multiple changes to TH dependent files in one second
        - Test for #32: Internal paths in error messages
        - Test for #50: Async problem with snippet interrupt
        - Test for fpco/fpco/#3043: constraints in monotypes
        - Fixed non-deterministic failure of test for #58 (#143)
        - Updated tests to support for 7.4 and 7.8 (mark allowable differences,
          use alpha-equivalence rather than syntactic identity to compare
          types, etc.), and be less picky about package versions
        - Others, testing most of the features and fixes in this release

     * Isolated a bug in sqlite which was causing #104.

     * Known issue: tracked #125 down to a bug in the GHC RTS, and reported
       this (#165, https://ghc.haskell.org/trac/ghc/ticket/8648). There is no
       fix yet though.

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
