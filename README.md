ide-backend
===========

Compile and test with

    cabal install --enable-tests

For some more testing, go to test/ and run

../dist/build/ghc-errors/ghc-errors

with each of the subdirectories in turn (sorry, no automation here yet).


The API is in IdeSession.hs. Format it nicely with

    cabal haddock


Fell free to file issues on the github tracker.
