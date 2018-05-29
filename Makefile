.POSIX:
EMACS = emacs
BATCH = $(EMACS) -batch -Q -L .

compile: thriter.elc thriter-tests.elc

test: check
check: thriter-tests.elc
	$(BATCH) -l thriter-tests.elc \
		 -f ert-run-tests-batch -f thriter-benchmark

clean:
	rm -f thriter.elc thriter-tests.elc

thriter-tests.elc: thriter-tests.el thriter.elc

.SUFFIXES: .el .elc
.el.elc:
	$(BATCH) -f batch-byte-compile $<
