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

 *  Version 0.8

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

       The two booleans indicate whether new variables should be found (cf.
       ghci's :print vs :sprint) and whether the value should be forced (:print
       vs :force). This is only valid during an active breakpoint.

       Known issues with debugging:

       - Resumed (:continue) snippets cannot be interrupted (#133)

     * Minor API changes:

        - RunActions now has kind * -> * ; what was previously just "RunActions"
          is now (from the client's perspective) "RunActions RunResult"
        - String argument to RunOk has been removed.

 *  Version 0.7.1

     * New functionality: types of subexpressions (#50). Known issues:

       - We sometimes report multiple types for the same span (see comments
         at the end of https://github.com/fpco/ide-backend/issues/50).

     * New functionality: report use sites of identifiers (#129)

     * New functionality: Generate .cabal files (#127)

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
