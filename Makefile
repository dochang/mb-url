CASK ?= cask
EMACS ?= emacs
VIRTUAL_EMACS = ${CASK} exec ${EMACS}

all:

install:
	${CASK} install

test:
	${VIRTUAL_EMACS} -Q -batch \
		-L . -L tests -l tests/mb-url-test.el \
		-f ert-run-tests-batch-and-exit
