# ide-backend Architecture

## Components

* **ide-backend**: a library, linked into the client program (like
  [stack-ide](https://github.com/commercialhaskell/stack-ide))

* **ide-backend-server**: an executable, invoked by the ide-backend
  library

* **ide-backend-common**: code shared between `ide-backend` and
  `ide-backend-server`.  Also used by
  [stack-ide's API](https://github.com/commercialhaskell/stack-ide/tree/master/stack-ide-api).

* **ide-backend-server/ide-backend-rts**: this is an internal package
  which provides some functions used for managing IO when running code
  in the interpreter.  It needs to be present in the environment used
  to run user code.  This is currently ensured by a clever mechanism -
  see [#289](https://github.com/fpco/ide-backend/issues/289).

## Support for multiple GHC versions

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

## How ide-backend works

FIXME

## How ide-backend-server works

FIXME
