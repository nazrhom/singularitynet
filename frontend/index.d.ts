export declare class Pool<T> {
  readonly config: ContractConfig;
  readonly args: T;

  deposit(amount: bigint, idxArray: int[]): Promise<int[]>;
  close(amount: bigint, idxArray: int[]): Promise<int[]>;
  userStake(amount: bigint): Promise<void>;
  userWithdraw(): Promise<void>;
}

// This is something of a hack for creating an opaque type without nominal typing,
// which typescript lacks
//
// It should not be possible to directly construct `ContractConfig`s
declare const cfg: unique symbol;
export type ContractConfig = typeof cfg;

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

// Bonded pool

export declare class BondedPool extends Pool<BondedPoolArgs> {}

export declare function createBondedPool(
  config: SdkConfig, initialArgs: InitialBondedArgs
): Promise<BondedPool>;

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

// Unbonded pool

export declare class UnbondedPool extends Pool<UnbondedPoolArgs> {}

export declare function createUnbondedPool(
  config: SdkConfig, initialArgs: InitialUnbondedArgs
): Promise<UnbondedPool>;

export type UnbondedPoolArgs = {
  start: bigint; // like POSIXTime so positive
  userLength: bigint; // like POSIXTime so positive
  bondingLength: bigint; // like POSIXTime so positive
  adminLength: bigint; // like POSIXTime so positive
  interestLength: bigint; // like POSIXTime so positive
  increments: bigint; // Natural
  interest: [bigint, bigint]; // Rational (positive)
  minStake: bigint; // Natural
  maxStake: bigint; // Natural
  unbondedAssetClass: [string, string]; // AssetClass ~ Tuple CBORCurrencySymbol TokenName
  admin: string; // PaymentPubKeyHash
  nftCs: string; // CBORHexCurrencySymbol
  assocListCs: string; // CBORHexCurrencySymbol
};

export type InitialUnbondedArgs = {
  start: bigint; // like POSIXTime so positive
  userLength: bigint; // like POSIXTime so positive
  adminLength: bigint; // like POSIXTime so positive
  interestLength: bigint; // like POSIXTime so positive
  bondingLength: bigint; // like POSIXTime so positive
  increments: bigint; // Natural
  interest: [bigint, bigint]; // Rational (positive)
  minStake: bigint; // Natural
  maxStake: bigint; // Natural
  unbondedAssetClass: [string, string]; // AssetClass ~ Tuple CBORHexCurrencySymbol TokenName
};
