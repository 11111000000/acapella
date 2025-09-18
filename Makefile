EMACS ?= $(shell command -v emacs)
BATCH = $(EMACS) -Q --batch -l tests/run-tests.el

.PHONY: test test-cask ci

test:
	$(BATCH)

test-cask:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask exec $(BATCH)

ci: test
