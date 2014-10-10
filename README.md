ghc-server
==========

A server interface to GHC. Work in progress. No official release yet.

### Purpose

Server that accepts commands and responds with structured data on
compiling, type info, compiler messages, etc. via s-expressions (and
possibly JSON in the future).

### Architecture

* Asynchonrous s-expression-based communication layer
* Sessions per project
* Possible to connect to remote instances over TCP
* Supports hsenv and sandboxes

### Features

* Type checking
* Interactive REPL
* Type info of top-level or sub-expressions
* Go to definition
* Kind info

See TODO.org for planned features.

### Major GHC releases supported

I test compilation against the following GHC versions:

* GHC 7.4
* GHC 7.8
* GHC 7.6

This is achieved via a
[wrapper module](https://github.com/chrisdone/ghc-server/blob/master/src/GHC/Compat.hs)
called `GHC.Compat` which wraps any function or type that has changed
between GHC versions.
