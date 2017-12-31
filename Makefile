STACKAGE=lts-9
STACK=stack --stack-yaml stack-$(STACKAGE).yaml

all:
	$(STACK) build --copy-bins --local-bin-path "$(shell pwd)/bin"

test:
	$(STACK) test

ghci:
	$(STACK) ghci

ghcid:
	ghcid -c "$(STACK) ghci"

yearly:
	m4 -DYEAR=$(shell date +%Y) yearly_summary.sql.m4 \
		| sqlite3 $(firstword $(wildcard athlete_*.sqlite)) \
		| perl -0777 -pe 's/(SELECT.*?;)/`tput setaf 246` . $$1 . `tput sgr0`/gse'
