((nil . ((ghc-session-startup . (lambda ()
                                  (ghc/set "-hide-package ghc-server")
                                  (ghc/set "-package ghc")
                                  (ghc/set "-isrc"))))))
