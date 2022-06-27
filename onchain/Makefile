FILTERSCRIPT := sed -n '1d;3d;s/.*\(".*"\).*/\1/;2p'

serialise-scripts:
	# Generate all scripts and print them to screen
	cabal build\
	&& cabal exec serialise -- state-policy -v -o /dev/stdout\
	&& cabal exec serialise -- list-policy -v -o /dev/stdout\
	&& cabal exec serialise -- validator -v -o /dev/stdout

serialise-scripts-offchain:
	cabal build
	cabal exec serialise -- state-policy -o /dev/stdout | ${FILTERSCRIPT} > ../offchain/scripts/BondedStateNFT.plutus
	cabal exec serialise -- list-policy -o /dev/stdout | ${FILTERSCRIPT} > ../offchain/scripts/BondedListNFT.plutus
	cabal exec serialise -- validator -o /dev/stdout | ${FILTERSCRIPT} > ../offchain/scripts/BondedPoolValidator.plutus

.PHONY: serialise-scripts
