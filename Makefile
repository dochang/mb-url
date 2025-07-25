all:

lint:
	eldev lint

test:
	eldev test --undercover on,codecov,restart,dontsend --undercover-report coverage.json

.PHONY: all lint test
