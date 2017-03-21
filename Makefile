emacs ?= emacs

LOAD = -l lastpass.el

.PHONY: all test clean

all: test

test:
	$(emacs) -batch $(LOAD) -f ert-run-tests-batch-and-exit

run:
	$(emacs) -q $(LOAD) -l targets/lastpass-init.el
	make clean

compile:
	$(emacs) -batch $(LOAD) -l targets/lastpass-init.el

clean:
	rm -f *.elc
