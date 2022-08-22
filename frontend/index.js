"use strict";

const frontend = import("./output.js");

exports.BondedPool = class BondedPool {
  constructor(config, args) {
    this.config = config;
    this.args = args;
  }

  async deposit(amount, idxArray) {
    const contracts = await frontend;
    return contracts.callDepositBondedPool(this.config)(this.args)(amount)(
      idxArray
    )();
  }

  async close(amount, idxArray) {
    const contracts = await frontend;
    return contracts.callCloseBondedPool(this.config)(this.args)(amount)(
      idxArray
    )();
  }

  async userStake(amount) {
    const contracts = await frontend;
    return contracts.callUserStakeBondedPool(this.config)(this.args)(amount)();
  }

  async userWithdraw() {
    const contracts = await frontend;
    return contracts.callUserWithdrawBondedPool(this.config)(this.args)();
  }
};

exports.UnbondedPool = class UnbondedPool {
  constructor(config, args) {
    this.config = config;
    this.args = args;
  }

  async deposit(amount, idxArray) {
    const contracts = await frontend;
    return contracts.callDepositUnbondedPool(this.config)(this.args)(amount)(
      idxArray
    )();
  }

  async close(amount, idxArray) {
    const contracts = await frontend;
    return contracts.callCloseUnbondedPool(this.config)(this.args)(amount)(
      idxArray
    )();
  }

  async userStake(amount) {
    const contracts = await frontend;
    return contracts.callUserStakeUnbondedPool(this.config)(this.args)(
      amount
    )();
  }

  async userWithdraw() {
    const contracts = await frontend;
    return contracts.callUserWithdrawUnbondedPool(this.config)(this.args)();
  }
};

exports.createBondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const bondedArgs = await contracts.callCreateBondedPool(config)(
    initialArgs
  )();
  return new exports.BondedPool(config, bondedArgs);
};

exports.createUnbondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const unbondedArgs = await callCreateUnbondedPool(config)(initialArgs)();
  return new exports.UnbondedPool(config, unbondedArgs);
};
