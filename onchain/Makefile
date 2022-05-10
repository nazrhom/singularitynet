SCRIPTS_PATH := ../frontend/src/Scripts

serialise-scripts:
	cabal build\
	&& cabal exec serialise -- state-policy -o ${SCRIPTS_PATH}/BondedStateNFT.js\
	&& cabal exec serialise -- list-policy -o ${SCRIPTS_PATH}/BondedListNFT.js\
	&& cabal exec serialise -- validator -o ${SCRIPTS_PATH}/BondedPoolValidator.js

.PHONY:
	serialise-scripts
