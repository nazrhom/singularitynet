"use strict";

const frontend = import("./output.js");

exports.buildContractConfig = async (sdkConfig) => {
  const contracts = await frontend;
  return contracts.buildContractConfig(sdkConfig)();
};

exports.callCreateBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callCreateBondedPool(config)(args)();
};

exports.callDepositBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callDepositBondedPool(config)(args)();
};

exports.callCloseBondedPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callCloseBondedPool(config)(args)();
};

exports.callUserStakeBondedPool = async (config, args, amount) => {
  const contracts = await frontend;
  return contracts.callUserStakeBondedPool(config)(args)(amount)();
};
