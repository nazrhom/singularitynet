export type ContractConfiguration = {
  serverHost: string,
  serverPort: bigint,
  serverSecure: boolean,
  ogmiosHost: string,
  ogmiosPort: bigint,
  ogmiosSecure: boolean,
  datumCacheHost: string,
  datumCachePort: bigint,
  datumCacheSecure: boolean,
  networkId: bigint,
  logLevel: any
};
export function mkContractConfiguration(
  serverHost: string,
  serverPort: int,
  serverSecure: boolean,
  ogmiosHost: string,
  ogmiosPort: String,
  ogmiosSecure: boolean,
  datumCacheHost: string,
  datumCachePort: bigint,
  datumCacheSecure: boolean,
  networkId: bigint,
  logLevel: any); ContractConfiguration;