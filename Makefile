.POSIX:
.SUFFIXES: .el .elc
EMACS = emacs

src = predd.el predd-tests.el
obj = $(src:.el=.elc)

all: test

compile: $(obj)

test: $(obj)
	$(EMACS) -Q -batch -L . -l predd-tests.elc \
		 -f ert-run-tests-batch \
		 -f predd-test-benchmark-print

clean:
	rm -rf $(obj)

predd.elc: predd.el
predd-tests.elc: predd-tests.el predd.elc

.el.elc:
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<
