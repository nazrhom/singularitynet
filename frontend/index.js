"use strict";

const frontend = import("./output.js");

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
