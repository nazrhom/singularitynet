export function callCreatePool(
  config: ContractConfiguration, args: InitialBondedArgs
):
  Promise<BondedPoolArgs>

export function callDepositPool(
  config: ContractConfiguration, args: BondedPoolArgs
):
  Promise<void>

export function callClosePool(
  config: ContractConfiguration, args: BondedPoolArgs
):
  Promise<void>

// TODO
export type ContractConfiguration = {}
export type InitialBondedArgs = {}
export type BondedPoolArgs = {}
