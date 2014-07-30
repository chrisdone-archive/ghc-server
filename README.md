ghc-server
==========

A server interface to GHC. Work in progress. No official release yet.

### Purpose

Generally it should replace use of GHC, GHCi, hlint, hdevtools, ghc-mod in one big
program that you can communicate with via a simple s-expression/JSON-based protocol.

### Features

* Asynchonrous s-expression-based communication layer.
* Compilation.
* Type checking.
* Loading modules.
* REPL evaluation.
* Type info.
* Kind info.
* Connecting to a remote instance.
* Setting compile flags.
* Setting package-conf flag.
* Capture stdout separately to evaluation.

See TODO.org for planned features.

### Major GHC releases supported

I test compilation against the following GHC versions:

* GHC 7.8
* GHC 7.6
* GHC 7.4

This is achieved via a
[wrapper module](https://github.com/chrisdone/ghc-server/blob/master/src/GHC/Compat.hs)
called `GHC.Compat` which wraps any function or type that has changed
between GHC versions.
