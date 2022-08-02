import {
  SdkConfig,
  BondedPool,
  InitialBondedArgs,
  BondedPoolArgs,
} from "singularitynet";

const singularitynet = require("singularitynet");
const bigInt = require("big-integer");

const main = async () => {
  const date = new Date();
  console.log(
    `Bonded pool creation: ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
  );
  const bondedPool: BondedPool = await singularitynet.createBondedPool(
    localSdkConfig,
    initialBondedArgs
  );

  const bondedPoolArgs: BondedPoolArgs = bondedPool.args;

  console.log("SWITCH WALLETS NOW - CHANGE TO USER 1");
  console.log("Waiting for pool start...");
  await countdownTo(Number(bondedPoolArgs.start));
};

const initialBondedArgs: InitialBondedArgs = {
  iterations: bigInt(2),
  start: bigInt(1000),
  end: bigInt(2000),
  userLength: bigInt(180000),
  bondingLength: bigInt(180000),
  interest: [bigInt(10), bigInt(100)],
  minStake: bigInt(1),
  maxStake: bigInt(50000),
  bondedAssetClass: [
    "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50",
    "AGIX",
  ],
};

const localSdkConfig: SdkConfig = {
  ctlServerConfig: {
    host: "localhost",
    port: 8081,
    secure: false,
    path: "",
  },
  ogmiosConfig: {
    host: "localhost",
    port: 1337,
    secure: false,
    path: "",
  },
  datumCacheConfig: {
    host: "localhost",
    port: 9999,
    secure: false,
    path: "",
  },
  networkId: 1,
  logLevel: "Info",
};

const countdownTo = async (tf: number) => {
  const now = Date.now();
  if (now > tf) {
    console.log("0");
  } else {
    console.log(`Countdown: ${showSecondsDiff(tf, now)}`);
    await sleep(10000);
  }
};

const sleep = async (ms: number) => new Promise((r) => setTimeout(r, ms));

const showSecondsDiff = (x: number, y: number) => (x - y) / 1000;

main()
  .then((_) => {})
  .catch((e) => console.log(e));
