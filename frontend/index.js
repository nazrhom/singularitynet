"use strict";

const frontend = import("./output.js");

exports.buildContractConfig = async (sdkConfig) => {
  const contracts = await frontend;
  return contracts.buildContractConfig(sdkConfig)();
};

exports.createBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callCreateBondedPool(config)(args)();
};

exports.depositBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callDepositBondedPool(config)(args)();
};

exports.closeBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callCloseBondedPool(config)(args)();
};

exports.userStakeBondedPool = async (config, args, amount) => {
  const contracts = await frontend;
  return contracts.callUserStakeBondedPool(config)(args)(amount)();
};

exports.userWithdrawBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callUserWithdrawBondedPool(config)(args)();
};
