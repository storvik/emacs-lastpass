EMACS ?= emacs
CASK  ?= cask
EMACSBATCH ?= $(EMACS) --batch -Q

SRCS = lastpass.el
TESTS = lastpass-tests.el

.PHONY: all deps test clean

all: test

deps:
	$(CASK) install

test: deps
	$(CASK) exec $(EMACSBATCH)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

run:
	$(EMACS) -q $(patsubst %,-l % , $(SRCS)) -l targets/lastpass-init.el
	make clean

compile:
	$(EMACS) -batch $(patsubst %,-l % , $(SRCS)) -l targets/lastpass-init.el

clean:
	rm -f *.elc
