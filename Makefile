EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile
TEST    := $(BATCH) -L tests -l multi-tests.elc -f ert-run-tests-batch

EL = multi.el multi-tests.el

ELC = $(EL:.el=.elc)

.PHONY : all compile test clean

all : test

compile: $(ELC)

test: compile
	$(TEST) -f multi-test-benchmark-print

clean:
	$(RM) *.elc

%.elc: %.el
	@$(COMPILE) $<
