.PHONY: all
all:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin"

.PHONY: test
test:
	stack test

.PHONY: ghci
ghci:
	stack ghci

.PHONY: ghcid
ghcid:
	ghcid --restart package.yaml -c "stack ghci"

.PHONY: yearly
yearly:
	m4 -DYEAR=$(shell date +%Y) yearly_summary.sql.m4 \
		| sqlite3 $(firstword $(wildcard athlete_*.sqlite)) \
		| perl -0777 -pe 's/(SELECT.*?;)/`tput setaf 246` . $$1 . `tput sgr0`/gse'
