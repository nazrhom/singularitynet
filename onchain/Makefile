FRONTEND_SCRIPTS_PATH := ../frontend/src/Scripts
OFFCHAIN_SCRIPTS_PATH := ../offchain/scripts

serialise-scripts:
	cabal build\
	&& cabal exec serialise -- state-policy -o ${FRONTEND_SCRIPTS_PATH}/BondedStateNFT.js\
	&& cabal exec serialise -- list-policy -o ${FRONTEND_SCRIPTS_PATH}/BondedListNFT.js\
	&& cabal exec serialise -- validator -o ${FRONTEND_SCRIPTS_PATH}/BondedPoolValidator.js
	cp ${FRONTEND_SCRIPTS_PATH}/*.js ${OFFCHAIN_SCRIPTS_PATH}

.PHONY: serialise-scripts
