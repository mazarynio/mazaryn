export const config = {
  port: process.env.PORT || 3020,
  solanaRpcUrl: process.env.SOLANA_RPC_URL || "https://api.devnet.solana.com",
  // solanaRpcUrl: "http://127.0.0.1:8899",-> quickly change to local validator or set to env
  commitment: "confirmed" as const,
  logLevel: process.env.LOG_LEVEL || "info",
};
