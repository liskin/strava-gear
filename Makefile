LTS=9
STACK=stack --stack-yaml stack-lts-$(LTS).yaml

all:
	$(STACK) build --copy-bins --local-bin-path "$(shell pwd)/bin"

test:
	$(STACK) test

ghci:
	$(STACK) ghci

ghcid:
	ghcid -c "$(STACK) ghci"
	
