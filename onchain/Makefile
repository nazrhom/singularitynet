serialise-scripts:
	# Generate all scripts and print them to screen
	cabal build\
	&& cabal exec serialise -- state-policy -v -o /dev/stdout\
	&& cabal exec serialise -- list-policy -v -o /dev/stdout\
	&& cabal exec serialise -- validator -v -o /dev/stdout

.PHONY: serialise-scripts
