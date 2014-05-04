ghc-server
==========

A server interface to GHC. Work in progress. No official release yet.

### Purpose

Generally it should replace use of GHC, GHCi, hlint, hdevtools, ghc-mod in one big 
program that you can communicate with via a simple s-expression/JSON-based protocol.

### Supported

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

### Planned / might do

* Sub-expression type info.
* Go to definition.
* Autocompletion.
* Additional JSON-based communication layer.
* Test suite against all supported GHC versions.
* Show generated Core for a module / function.
* Show generated CMM for a module / function.
* Show generated assembly for a module / function.
* Hlint checking.
* Hoogle generation.
* Jump to documentation.
* Structured-haskell-mode parser.
* Haskell-names-based scope resolution.
* Capture stderr and stdout separately to evaluation.
* Figure out compile flags from Cabal file.
* Support presentations (via `present` package).
* Programmatic access to scope, type info, instances, etc.
* Managed handling of Cabal configuration.
* Make better debugging available.
* Watch statements/variables.
* Automatic import management.
* Whatever I feel like doing.

### Major GHC releases supported

I test compilation against the following GHC versions.

* GHC 7.8
* GHC 7.6 
* GHC 7.4 
* GHC 7.2

This is achieved via a [wrapper module](https://github.com/chrisdone/ghc-server/blob/master/src/GHC/Compat.hs) 
called `GHC.Compat` which wraps any function or type that
has changed between GHC versions.

### What about GHCi, Scion, hdevtools, ghc-mod, FP Haskell Center, etc.?

There are many approaches to this problem. Consider this a vanity project
on par with Frank Sinatra's _I did it My Way…_. As an Emacs user I'm 
very particular about my development environment. I change it from day to day
as I please, sometimes with bad ideas, sometimes with good ideas. Here
I have the freedom to make the Haskell setup that I want. I will treat
the GHC library like XMonad's library, and make a program out of it.

As always, when taking inspiration for a development environment, one should
always look at the Lisp world. Here, SLIME is my inspiration. The SBCL compiler
exposes a wealth of information to the user. More than anyone could ever want.
SBCL makes sense of that information and gives the user as much as is asked for.
