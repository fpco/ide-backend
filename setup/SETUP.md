IDE backend requirements for ghc/package environments
=====================================================

Now that the IDE backend supports multiple versions of ghc, there is a greater
complexity in the requirements on the various ghc and package environments
needed to build it and run it.

We will try to document here what those requirements are, both abstractly and
a specific recipe using (one flavour of) sandboxes.


General points
--------------

Firstly, let us look at the IDE backend components:

 * ide-backend: a library, linked into the isolation-runner
 * ide-backend-server: an exe, invoked by the ide-backend library
 * ide-backend-rts: a library, loaded at runtime by the ide-backend-server

The other important components here are ghc itself (various versions, some
patches), and the many library packages available to the end users (stackage
etc).

The IDE backend architecture supports multiple versions of ghc by having
multiple instances of the ide-backend-server, one instance built against each
supported ghc. The ide-backend library can then talk to one of these
executables.

So **note**: the ide-backend library does not have to be built with the same
version of ghc as the ide-backend-server; indeed when supporting multiple ghc
versions that would be impossible.

The ide-backend-server *must* be built with the same ghc and set of libraries
as it will use at runtime. This is because (like ghci) it has to load these
libraries at runtime. (Technically, it may be possible to use a set of
libraries at runtime built by a different instance of the same version of ghc,
however this is playing with fire.)

The ide-backend-server must be built with a patched version of ghc (that
provides some extra hooks the IDE needs).

Thus we have the requirement that all the user packages also be built with this
patched ghc. (Again, it may be possible to use a different instance of the same
version of ghc, but we cannot guarantee ABI compatibility in such a setup.)


Environments
------------

So we can identify three separate environments:

 * build environment for ide-backend + isolation-runner
 * user environment with ghc 7.4.x
 * user environment with ghc 7.8.x

The build environment for ide-backend + isolation-runner could be the same as
one of the user environments, but it does not have to be. For example it could
use a stock ghc-7.6.


Build environment for ide-backend
---------------------------------

This environment is relatively straightforward. It can use a stock (ie
unpatched) ghc. The ide-backend does not depend on the GHC library and because
of that it has (in principle) relatively flexible dependencies. We have of
course been testing ide-backend with ghc-7.4, but in principle it could work
with any other version, possibly with a little fairly ordinary porting. For
example, if it made technical sense for the isolation-runner to use ghc-7.6,
then that would be no problem for ide-backend.


User environment with ghc 7.4.x
-------------------------------

The instance of the ide-backend-server used at runtime for the ghc 7.4.x user
environment must of course be compatible with this environment, because it will
at runtime load packages from it. The only way to guarantee this is to build it
in this environment.

Building ide-backend-server does require the patched ghc instance. So this
implies the full environment must be built with this patched ghc instance, not
a stock ghc 7.4.

An instance of ide-backend-rts must be available in this environment so that
the ide-backend-server instance can use it.

However, the build-time dependencies of ide-backend-server do not need be
present in the environment at runtime. In practice this means that while it
must be built with the same ghc and global package db, its extra build time
dependencies can live in a separate package db that is not made available at
runtime.


User environment with ghc 7.8.x
-------------------------------

Similar points as above apply.
Ghc 7.8 snapshot still needs some patches. The same point as above applies
about building the user packages with this ghc instance.


$PATH setup
-----------

At runtime, when using the ide-backend library, the library has to be able to
find both ghc and the compatible instance of ide-backend-server.

The ide-backend library can be instructed to search extra dirs in addition to
the process $PATH. To support multiple different ghc versions this mechanism
must be used. Set up the $PATH so that ghc and ide-backend-server are *not*
present, and then pass the extra search dirs when initialising and IdeSession.
This will allow multiple IdeSessions in a single isolation-runner process to
use different ghc versions.


More details and worked example using sandboxes
===============================================

In order to make sure that ide-backend works even when completely isolated from
ide-backend-server (after all, we want different ide-backend-servers for
different versions of ghc, each with their own package DBs), these instructions
assume the sandbox approach described in
http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/ ; we summarize
the approach below.  It is of course not required to follow that approach; if
using a different approach, these instructions can still help settings things
up, mutatis mutandis.

We have various "sandboxes" (build environments) represented by directories

    ~/env/fpco-stock-7.4
    ~/env/fpco-patched-7.4
    ~/env/fpco-patched-7.8

a symlink

    ~/env/active -> ~/env/fpco-stock-7.4

(or whichever sandbox is "active"). Then we have (non-changing) symlinks

    ~/.ghc   -> ~/env/active/dot-ghc
    ~/.cabal -> ~/env/active/dot-cabal
    ~/local  -> ~/env/active/local

The fpco-stock-7.4 sandbox
--------------------------

This is the sandbox in which we compile the ide-backend client, and the
infrastructure surrounding it. The version of ghc here is not so important;
this could be ghc 7.6 for instance (i.e., it is unrelated to the version that
we use for ide-backend-server). For now we will assume it's a stock 7.4.

* If you want to profile, you may want to set

      library-profiling: True

  in your ~/.cabal/config.

* Install:

  - Standard installation of ghc 7.4.2
  - ide-backend/vendor/cabal/Cabal (Cabal-ide-backend) and
    ide-backend/vendor/binary (binary-ide-backend)
    (make sure to ghc-pkg hide these packages)
  - ide-backend and dependencies

The fpco-patched-7.4 and fpco-patched-7.8 sandboxes
---------------------------------------------------

The setup instructions for these two sandboxes are almost the same; there are
only a few minor differences, explained when they come up.

* Set

      documentation: True

  in ~/.cabal/config so that .haddock files are created (these are necessary to
  so that ide-backend-server can create more informative identifier
  information).

  (There is probably not much point in installing profiling libraries in this
  sandbox because the ghc api is not useable in profiling mode.)

* Since we will want to reference tools (such as haddock) and files (such as
  package registration files to locate the library binaries) from this sandbox
  while another sandbox is active, we have to make sure that the "prefix"
  picked by Cabal should not include symlinks which may point to one location
  at installation time and another at runtime. It is therefore a good idea to
  set

      install-dirs user
        prefix: /Users/dev/env/fpco-patched-7.4/dot-cabal

  in your ~/.cabal/config (or whatever the absolute path is).

  (See http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/ , the
  last section, "Known Limitations".)

* For ghc 7.4 and 7.8 install

  - Branch "ide-backend-experimental-74" of ghc (see instructions below)
    or branch "ide-backend-experimental-78", respectively
  - ide-backend/vendor/binary (binary-ide-backend)
    (make sure to ghc-pkg hide binary-ide-backend)
  - ide-backend-server and dependencies (including alex/happy)

* Create package DB for snippets (the "snippet DB")

  The snippet DB is logically independent of the DB used to build
  ide-backend-server: the dependencies of ide-backend-server do not have to be
  available when ide-backend-server runs. (Moreover, it is conceivable that
  ide-backend-server relies on version X of a particular package, but we want
  to make version Y available in the snippet DB.)

  However, the packages in the snippet DB should be compiled with the patched
  ghc (ghci can only load guarantee to libs built by the same ghc instance -- a
  different instance of the same ghc version may work but you're playing with
  fire).

  To initialize the snippet DB use

      ghc-pkg init /Users/dev/env/fpco-patched-7.4/dot-ghc/snippet-db

  To install packages into the snippet DB use

      cabal install --package-db=clear \
                    --package-db=global \
                    --package-db=/Users/dev/env/fpco-patched-7.4/dot-ghc/snippet-db

  (modifying absolute paths as required, of course).  Clearing the package DB
  is necessary so that we do not rely on any dependencies in the user DB (which
  will not be available when ide-backend-server runs).

  You will want to install

  - ide-backend/rts (required)
  - the test suite requires the following packages (you might of course want to
    use a separate snippet DB for the test suite:)

    * parallel-3.2.0.4
    * mtl
    * monads-tf-0.1.0.1
    * yesod-1.2.4 (optional; only required for one test; install with
      cabal --max-backjumps=-1; do not attempt for 7.8 for now))
    * parsec-3.1.3 (optional; only required for one test)

    The tests are no longer as picky as they used to be about the versions of
    these packages; if you are getting test failures with different versions,
    you should file a bug.
  - whatever other packages you want to be available to snippets at runtime

Running the tests
=================

The tests (as well as the ide-backend client library itself) will be compiled
in the fpco-stock-7.4 sandbox, and can be run inside that sandbox (or indeed in
an empty sandbox where no ghc compiler is available on the path at all), as
long as we specify the right paths:

    IDE_BACKEND_EXTRA_PATH_DIRS=~/env/fpco-patched-7.4/local/bin:~/env/fpco-patched-7.4/dot-cabal/bin \
    IDE_BACKEND_PACKAGE_DB=~/env/fpco-patched-7.4/dot-ghc/snippet-db \
    dist/build/ghc-errors/ghc-errors

(These environment variables are translated by the test-suite to the
corresponding options in SessionConfig, they are *not* part of the ide-backend
library itself.)

Installing the patched versions of ghc
======================================

In order to build ghc, you need ghc; both ghc 7.4 and ghc 7.8 can be built
using the stock 7.4; if using the sandboxes approach, you will want a sandbox
active that contains a stock 7.4 compiler (fpco-stock-7.4 will do).

ghc 7.4
-------

* Get ghc from fpco; in ~/env/fpco-patched-7.4/local/src, run

      git clone git@github.com:fpco/ghc

* Go to the ghc directory, and checkout the ide-backend branch of 7.4.2:

      git checkout ide-backend-experimental-74

* Get the 7.4.2 release of the core libraries:

```
      ./sync-all --no-dph -r git://git.haskell.org get
      ./sync-all checkout -b ghc-7.4.2 ghc-7.4.2-release
```

* Make sure we're still in the experimental branch of ghc:

      git checkout ide-backend-experimental-74

* Create build.mk

      cp mk/build.mk.sample mk/build.mk

  select the quick BuildFlavour

      BuildFlavour = quick

  and make sure haddocks get built by setting

      HADDOCK_DOCS = YES

  in the section for the "quick" build flavour (make sure there are no trailing
  spaces in your build.mk).

* Build as usual

      perl boot && ./configure && make -j8

  (Note on OSX Mavericks: use gcc-4.8 (or maybe gcc-4.2) instead, and disable
  DTRACE:

      export CC=`which gcc-4.8`
      perl boot && ./configure && make -j8 USE_DTRACE=NO

  If you don't want to disable DTRACE, it might work to cherry-pick
  https://github.com/ghc/ghc/commit/8878541d02ad15fbdbd04608cbc6ea3fde5d5beb;
  YMMV. See also https://gist.github.com/cartazio/7131371).

* Make the in-place compiler available as normal; i.e. create the following
  symlinks in ~/env/fpco-patched-7.4/local/bin:

      ghc                 -> ../src/ghc/inplace/bin/ghc-stage2
      ghc-7.4.2.20140313  -> ../src/ghc/inplace/bin/ghc-stage2
      ghc-pkg             -> ../src/ghc/inplace/bin/ghc-pkg
      haddock             -> ../src/ghc/inplace/bin/haddock
      hsc2hs              -> ../src/ghc/inplace/bin/hsc2hs

ghc 7.8
-------

The instructions for 7.8 are much as they are for 7.4,
though it may be more troublesome to get the right versions of libraries
(TODO: ideally, we'd be using a specific snapshot, but this is made
a bit awkward by the fact that ghc does not make proper use of git subrepos).

* Get ghc from fpco; in ~/env/fpco-patched-7.8/local/src, run

      git clone git@github.com:fpco/ghc

* Go to the ghc directory, and checkout the ide-backend branch of 7.8:

      git checkout ide-backend-experimental-78

* Get the latest version of the core libraries:

      ./sync-all --extra --nofib -r git://git.haskell.org get -b ghc-7.8

* Make sure we're still in the experimental branch of ghc:

      git checkout ide-backend-experimental-78

* Create build.mk

      cp mk/build.mk.sample mk/build.mk

  select the quick BuildFlavour

      BuildFlavour = quick

  and make sure haddocks get built by setting

      HADDOCK_DOCS = YES

  in the section for the "quick" build flavour (make sure there are no trailing
  spaces in your build.mk).

* Build as usual

      perl boot && ./configure && make -j8

  (Note on OSX Mavericks: use gcc-4.8 (or maybe gcc-4.2) instead:

      export CC=`which gcc-4.8`
      perl boot && ./configure && make -j8

  The DTRACE patch mentioned in the instructions for 7.4 is already in ghc 7.8.
  See also https://gist.github.com/cartazio/7131371).

* Make the in-place compiler available as normal; i.e. create the following
  symlinks in ~/env/fpco-patched-7.8/local/bin:

      ghc              -> ../src/ghc/inplace/bin/ghc-stage2
      ghc-7.8.1.20140411 -> ../src/ghc/inplace/bin/ghc-stage2
      ghc-pkg          -> ../src/ghc/inplace/bin/ghc-pkg
      haddock          -> ../src/ghc/inplace/bin/haddock
      hsc2hs           -> ../src/ghc/inplace/bin/hsc2hs

  (TODO: version number will change)
