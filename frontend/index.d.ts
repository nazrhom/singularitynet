// This is something of a hack for creating an opaque type without nominal typing,
// which typescript lacks
//
// It should not be possible to directly construct `ContractConfig`s
declare const cfg: unique symbol;
export type ContractConfig = typeof cfg;

export declare function buildContractConfig(
  config: SdkConfig
): Promise<ContractConfig>;

export declare class BondedPoolContracts {
  readonly config: ContractConfig;
  readonly args: BondedPoolArgs;

  create(): Promise<BondedPoolArgs>;
  deposit(): Promise<void>;
  close(): Promise<void>;
  userStake(amount: bigint): Promise<void>;
  userWithdraw(): Promise<void>;
}

export type LogLevel = "Trace" | "Debug" | "Info" | "Warn" | "Error"

export type NetworkId = 1 | 2

export type SdkServerConfig = {
  host: string; // e.g. "localhost"
  port: number; // uint
  secure: boolean;
};

export type SdkConfig = {
  ctlServerConfig: SdkServerConfig;
  ogmiosServerConfig: SdkServerConfig;
  datumCacheConfig: SdkServerConfig;
  networkId: NetworkId; // int
  logLevel: LogLevel;
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
