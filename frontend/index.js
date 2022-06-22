"use strict";

const frontend = import("./output.js");

exports.createBondedPool = async (sdkConfig, intialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const bondedArgs = await callCreateBondedPool(config)(initialArgs)();
  return new BondedPool(config, bondedArgs);
};

exports.BondedPool = class BondedPool {
  constructor(config, args) {
    this.config = config;
    this.args = args;
  }

  async deposit() {
    const contracts = await frontend;
    return contracts.callDepositBondedPool(this.config)(this.args)();
  }

  async close() {
    const contracts = await frontend;
    return contracts.callCloseBondedPool(this.config)(this.args)();
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

exports.createUnbondedPool = async (sdkConfig, intialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const unbondedArgs = await callCreateUnbondedPool(config)(initialArgs)();
  return new UnbondedPool(config, unbondedArgs);
};

exports.UnbondedPool = class UnbondedPool {
  constructor(config, args) {
    this.config = config;
    this.args = args;
  }

  async deposit() {
    const contracts = await frontend;
    return contracts.callDepositUnbondedPool(this.config)(this.args)();
  }

  async close() {
    const contracts = await frontend;
    return contracts.callCloseUnbondedPool(this.config)(this.args)();
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
