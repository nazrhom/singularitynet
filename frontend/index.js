"use strict";

const frontend = import("./output.js");

exports.BondedPool = class BondedPool {
  constructor(config, args, address) {
    this.config = config;
    this.args = args;
    this.address = address;
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
  constructor(config, args, address) {
    this.config = config;
    this.args = args;
    this.address = address;
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
  const info = await contracts.callCreateBondedPool(config)(initialArgs)();
  return new exports.BondedPool(config, info.args, info.address);
};

exports.createUnbondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const info = await contracts.callCreateUnbondedPool(config)(initialArgs)();
  return new exports.UnbondedPool(config, info.args, info.address);
};

exports.getNodeTime = async (sdkConfig) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const time = await contracts.callGetNodeTime(config)();
  return time;
};
