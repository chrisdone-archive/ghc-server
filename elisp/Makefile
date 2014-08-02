ELFILES = ghc-macros.el \
          ghc-session.el \
	  ghc-con.el \
	  ghc-repl.el \
	  ghc-mode.el \
	  ghc-ident.el \
	  ghc-log.el \
	  ghc-msgs.el \
	  ghc-cmd.el \
	  ghc.el

ELCFILES = $(ELFILES:.el=.elc)

all: $(ELCFILES)

%.elc: %.el
	@emacs -Q -L . \
		--batch \
		--eval "(byte-compile-disable-warning 'cl-functions)" \
		-f batch-byte-compile \
		$<

clean:
	rm -f *.elc
