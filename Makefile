EMACS   ?= emacs
BATCH   := $(EMACS) -batch -Q -L .
COMPILE := $(BATCH) -f batch-byte-compile
TEST    := $(BATCH) -L tests -l predd-tests.elc -f ert-run-tests-batch

EL = predd.el predd-tests.el

ELC = $(EL:.el=.elc)

.PHONY : all compile test clean

all : test

compile: $(ELC)

test: compile
	$(TEST) -f predd-test-benchmark-print

clean:
	$(RM) *.elc

%.elc: %.el
	@$(COMPILE) $<
