"use strict";

const frontend = import("./output.js");

exports.callCreatePool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callCreatePool(config)(args)();
};

exports.callDepositPool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callDepositPool(config)(args)();
};

exports.callClosePool = async (config, args) => {
  const contracts = await frontend;
  return contracts.callClosePool(config)(args)();
};
