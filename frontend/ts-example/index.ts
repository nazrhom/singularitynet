// This module demonstrates how to use the `singularitynet` TS/JS SDK to
// operate a bonded pool across the entire application lifecycle

import {
  SdkConfig,
  BondedPool,
  InitialBondedArgs,
  BondedPoolArgs,
} from "singularitynet";

const singularitynet = require("singularitynet");
// We need to use `big-integer` directly instead of the `BigInt` constructor
// as this package defines some methods that are required by the PS code
const BigInteger = require("big-integer");

// Runs all of the operations defined for a `BondedPool` and simulates the
// entire pool lifecycle
//
// Make sure to switch wallets as directed
const main = async () => {
  // some helpers for logging directions to switch wallets
  const admin = "ADMIN";
  const user = "USER";

  // Admin creates pool
  console.log(`STARTING AS ${admin}`);
  const nodeTime = await singularitynet.getNodeTime(localHostSdkConfig);
  console.log(nodeTime);
  const date = new Date(nodeTime);
  const delay = BigInteger(0);
  console.log(
    `Bonded pool creation: ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
  );

  // The initial arguments of the pool. The rest of the parameters are obtained
  // during pool creation.
  const initialBondedArgs: InitialBondedArgs = {
    iterations: BigInteger(1),
    start: nodeTime.add(delay),
    end: nodeTime.add(delay).add(BigInteger(1).multiply(BigInteger(360000))).add(180000),
    userLength: BigInteger(180000),
    bondingLength: BigInteger(180000),
    interest: { numerator: BigInteger(10), denominator: BigInteger(100) },
    minStake: BigInteger(1),
    maxStake: BigInteger(50000),
    bondedAssetClass: {
      currencySymbol:
        "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50",
      tokenName: "AGIX",
    },
  };

  const bondedPool: BondedPool = await singularitynet.createBondedPool(
    localHostSdkConfig,
    initialBondedArgs
  );
  const bondedPoolArgs: BondedPoolArgs = bondedPool.args;
  await logSwitchAndCountdown(user, "pool start", bondedPoolArgs.start);

  // User stakes, waiting for pool start
  const userStakeAmt = BigInteger(40000);
  await bondedPool.userStake(userStakeAmt);
  await logSwitchAndCountdown(
    admin,
    "bonding period",
    bondedPoolArgs.start + bondedPoolArgs.userLength
  );

  // Admin deposits to pool
  const depositBatchSize = BigInteger(1);
  await bondedPool.deposit(depositBatchSize, []);
  await logSwitchAndCountdown(
    user,
    "withdrawing  period",
    bondedPoolArgs.start +
      bondedPoolArgs.userLength +
      bondedPoolArgs.bondingLength
  );

  // User withdraws
  await bondedPool.userWithdraw();
  await logSwitchAndCountdown(admin, "closing period", bondedPoolArgs.end);

  // Admin closes pool
  const closeBatchSize = BigInteger(10);
  await bondedPool.close(closeBatchSize, []);

  console.log("Pool closed");
};

const localHostSdkConfig: SdkConfig = {
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
  networkId: 0,
  logLevel: "Info",
  walletSpec: "Lode",
};

// Helpers

const logSwitchAndCountdown = async (
  who: "USER" | "ADMIN", // the wallet to switch to
  what: string, // what we're waiting for
  time: BigInteger // how long to wait
) => {
  console.log(`SWITCH WALLETS NOW - CHANGE TO ${who}`);
  const _input = prompt("Press OK after switching.");
  console.log(`Waiting for ${what}...`);
  await countdownTo(Number(time));
};

const countdownTo = async (tf: number) => {
  let now = await singularitynet.getNodeTime(localHostSdkConfig);
  while (now <= tf) {
    console.log(`${now} <= ${tf}`);
    console.log(`Countdown: ${showSecondsDiff(tf, now)}`);
    await sleep(10000);
    now = await singularitynet.getNodeTime(localHostSdkConfig);
  }
  console.log(`${now} > ${tf}`);
  console.log(`0`);
};

const sleep = async (ms: number) => new Promise((r) => setTimeout(r, ms));

const showSecondsDiff = (x: number, y: number) => (x - y) / 1000;

// Run the contracts, see `main` above
main()
  .then((_) => {})
  .catch((e) => console.log(e));
