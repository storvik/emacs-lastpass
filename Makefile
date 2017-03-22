EMACS ?= emacs
CASK  ?= cask

LOAD = -l lastpass.el

.PHONY: all deps test clean

all: test

deps:
	$(CASK) install

test: deps
	$(CASK) exec $(EMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

run:
	$(EMACS) -q $(LOAD) -l targets/lastpass-init.el
	make clean

compile:
	$(EMACS) -batch $(LOAD) -l targets/lastpass-init.el

clean:
	rm -f *.elc
