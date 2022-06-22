"use strict";

const frontend = import("./output.js");

exports.buildContractConfig = async (sdkConfig) => {
  const contracts = await frontend;
  return contracts.buildContractConfig(sdkConfig)();
};

exports.BondedPool = class BondedPool {
  constructor(config, args) {
    this.config = config;
    this.args = args;
  }

  async create() {
    const contracts = await frontend;
    return contracts.callCreateBondedPool(this.config)(args)();
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
