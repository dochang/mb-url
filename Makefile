CASK ?= cask
EMACS ?= emacs
VIRTUAL_EMACS = ${CASK} exec ${EMACS}

all:

test:
	${VIRTUAL_EMACS} -Q -batch \
		-L . -L tests -l tests/test-mb-url.el \
		-f ert-run-tests-batch-and-exit
