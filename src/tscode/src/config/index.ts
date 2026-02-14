export const config = {
  port: process.env.PORT || 3020,
  solanaRpcUrl: process.env.SOLANA_RPC_URL || "https://api.devnet.solana.com",
  commitment: "confirmed" as const,
  logLevel: process.env.LOG_LEVEL || "info",
};
