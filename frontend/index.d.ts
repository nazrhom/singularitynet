// This is something of a hack for creating an opaque type without nominal typing,
// which typescript lacks
//
// It should not be possible to directly construct `ContractConfig`s
declare const cfg: unique symbol;
export type ContractConfig = typeof cfg;

export function buildContractConfig(config: SdkConfig): Promise<ContractConfig>

export function callCreateBondedPool(
  config: ContractConfig, args: InitialBondedArgs
):
  Promise<BondedPoolArgs>

export function callDepositBondedPool(
  config: ContractConfig, args: BondedPoolArgs
):
  Promise<void>

export function callCloseBondedPool(
  config: ContractConfig, args: BondedPoolArgs
):
  Promise<void>

export function callUserStakeBondedPool(
  config: ContractConfig, args: BondedPoolArgs
):
  Promise<void>

export type SdkConfig = {
  serverHost: string; // e.g. "localhost"
  serverPort: number; // uint
  serverSecure: boolean;
  ogmiosHost: string; // e.g. "localhost"
  ogmiosPort: number; // uint
  ogmiosSecure: boolean;
  datumCacheHost: string; // e.g. "localhost"
  datumCachePort: number; // uint
  datumCacheSecure: boolean;
  networkId: number; // int
  logLevel: string; // "Trace", "Debug", "Info", "Warn", "Error"
};

export type InitialBondedArgs = {
  iterations: bigint; // Natural
  start: bigint; // like POSIXTime so positive
  end: bigint; // like POSIXTime so positive
  userLength: bigint; // like POSIXTime so positive
  bondingLength: bigint; // like POSIXTime so positive
  interest: [bigint, bigint]; // Rational (positive)
  minStake: bigint; // Natural
  maxStake: bigint; // Natural
  bondedAssetClass: [string, string]; // AssetClass ~ Tuple CBORHexCurrencySymbol TokenName
};

export type BondedPoolArgs = {
  iterations: bigint; // Natural
  start: bigint; // like POSIXTime so positive
  end: bigint; // like POSIXTime so positive
  userLength: bigint; // like POSIXTime so positive
  bondingLength: bigint; // like POSIXTime so positive
  interest: [bigint, bigint]; // Rational (positive)
  minStake: bigint; // Natural
  maxStake: bigint; // Natural
  bondedAssetClass: [string, string]; // AssetClass ~ Tuple CBORCurrencySymbol TokenName
  admin: string; // PaymentPubKeyHash
  nftCs: string; // CBORHexCurrencySymbol
  assocListCs: string; // CBORHexCurrencySymbol
};
