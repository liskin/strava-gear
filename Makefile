.PHONY: all
all:
	stack build --copy-bins --local-bin-path "$(shell pwd)/bin"

.PHONY: test
test:
	stack test strava-gear:hspec

.PHONY: ghci
ghci:
	stack ghci strava-gear:lib

.PHONY: ghcid
ghcid:
	ghcid --restart package.yaml -c "stack ghci strava-gear:lib"

.PHONY: yearly
yearly: YEAR=$(shell date +%Y)
yearly:
	m4 -DYEAR=$(YEAR) yearly_summary.sql.m4 \
		| sqlite3 $(firstword $(wildcard athlete_*.sqlite)) \
		| perl -0777 -pe 's/(SELECT.*?;)/`tput setaf 246` . $$1 . `tput sgr0`/gse'
