exports.mkContractConfiguration =
  serverHost =>
  serverPort =>
  serverSecure =>
  ogmiosHost =>
  ogmiosPort =>
  ogmiosSecure =>
  datumCacheHost =>
  datumCachePort =>
  datumCacheSecure =>
  networkId =>
  logLevel =>
  {
    return {
      serverHost: serverHost,
      serverPort: serverPort,
      serverSecure: serverSecure,
      ogmiosHost: ogmiosHost,
      ogmiosPort: ogmiosPort,
      ogmiosSecure: ogmiosSecure,
      datumCacheHost: datumCacheHost,
      datumCachePort: datumCachePort,
      datumCacheSecure: datumCacheSecure,
      networkId: networkId,
      logLevel: logLevel
  };
};