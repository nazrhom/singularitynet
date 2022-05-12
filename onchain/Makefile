FRONTEND_SCRIPTS_PATH := ../frontend/src/Scripts
OFFCHAIN_SCRIPTS_PATH := ../offchain/scripts

serialise-scripts:
	# Generate all scripts
	cabal build\
	&& cabal exec serialise -- state-policy -o ${FRONTEND_SCRIPTS_PATH}/BondedStateNFT.js\
	&& cabal exec serialise -- list-policy -o ${FRONTEND_SCRIPTS_PATH}/BondedListNFT.js\
	&& cabal exec serialise -- validator -o ${FRONTEND_SCRIPTS_PATH}/BondedPoolValidator.js
	# Copy just the serialised script to offchain, without the JS wrapper
	for file in ${FRONTEND_SCRIPTS_PATH}/*.js ; do                  \
		target=${OFFCHAIN_SCRIPTS_PATH}/$$(basename -s .js $${file}).plutus; \
		sed '1d;3d;s/.*\(".*"\).*/\1/' $${file} > $${target};       \
	done

.PHONY: serialise-scripts
