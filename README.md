ide-backend
===========

Compile and test with

    cabal install --enable-tests


The API is in IdeSession.hs. It can be formatted nicely for viewing with

    cabal haddock

A sample program using the ide-backend library can be invoked as follows

    dist/build/typecheck-dir/typecheck-dir

Feel free to file issues on the github tracker.

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
