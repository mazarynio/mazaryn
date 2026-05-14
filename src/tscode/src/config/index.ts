import dotenv from "dotenv";
dotenv.config();

interface Config {
  nodeEnv: string;
  port: number;
  solanaRpcUrl: string;
  nearRpcUrl: string;
  nearFallbackRpcUrls: string[];
  nearNetwork: string;
  aptosNetwork: string;
  aptosNodeUrl: string;
  aptosFaucetUrl: string;
  aptosDatabasePath: string;
  encryptionKey: string;
  jwtSecret: string;
  databasePath: string;
  nearDatabasePath: string;
  maxLoginAttempts: number;
  accountLockoutDuration: number;
  rateLimitWindowMs: number;
  rateLimitMaxRequests: number;
  sessionTimeout: number;
  enable2FA: boolean;
  auditLogEnabled: boolean;
  auditLogPath: string;
  corsAllowedOrigins: string[];
}

function validateConfig(): Config {
  const encryptionKey = process.env.ENCRYPTION_KEY;
  const jwtSecret = process.env.JWT_SECRET;

  if (!encryptionKey || encryptionKey.length < 32) {
    throw new Error("ENCRYPTION_KEY must be at least 32 characters long");
  }

  if (!jwtSecret || jwtSecret.length < 32) {
    throw new Error("JWT_SECRET must be at least 32 characters long");
  }

  const nearNetwork = process.env.NEAR_NETWORK || "testnet";

  if (!["testnet", "mainnet"].includes(nearNetwork)) {
    throw new Error("NEAR_NETWORK must be either testnet or mainnet");
  }

  const defaultNearRpc =
    nearNetwork === "mainnet"
      ? "https://rpc.mainnet.near.org"
      : "https://test.rpc.fastnear.com";

  const fallbackRpcRaw = process.env.NEAR_FALLBACK_RPC_URLS ?? "";
  const nearFallbackRpcUrls = fallbackRpcRaw
    .split(",")
    .map((u) => u.trim())
    .filter(Boolean);

  const aptosNetwork = process.env.APTOS_NETWORK || "testnet";

  if (!["mainnet", "testnet", "devnet", "local"].includes(aptosNetwork)) {
    throw new Error("APTOS_NETWORK must be mainnet, testnet, devnet, or local");
  }

  const aptosNodeUrlDefaults: Record<string, string> = {
    mainnet: "https://fullnode.mainnet.aptoslabs.com/v1",
    testnet: "https://fullnode.testnet.aptoslabs.com/v1",
    devnet: "https://fullnode.devnet.aptoslabs.com/v1",
    local: "http://127.0.0.1:8080/v1",
  };

  const aptosFaucetUrlDefaults: Record<string, string> = {
    mainnet: "",
    testnet: "https://faucet.testnet.aptoslabs.com",
    devnet: "https://faucet.devnet.aptoslabs.com",
    local: "http://127.0.0.1:8081",
  };

  return {
    nodeEnv: process.env.NODE_ENV || "development",
    port: parseInt(process.env.PORT || "3020", 10),
    solanaRpcUrl: process.env.SOLANA_RPC_URL || "https://api.devnet.solana.com",
    nearRpcUrl: process.env.NEAR_RPC_URL || defaultNearRpc,
    nearFallbackRpcUrls,
    nearNetwork,
    aptosNetwork,
    aptosNodeUrl:
      process.env.APTOS_NODE_URL || aptosNodeUrlDefaults[aptosNetwork],
    aptosFaucetUrl:
      process.env.APTOS_FAUCET_URL || aptosFaucetUrlDefaults[aptosNetwork],
    aptosDatabasePath: process.env.APTOS_DB_PATH || "./data/aptos-wallets.db",
    encryptionKey,
    jwtSecret,
    databasePath: process.env.DATABASE_PATH || "./data/wallets.db",
    nearDatabasePath: process.env.NEAR_DB_PATH || "./data/near-wallets.db",
    maxLoginAttempts: parseInt(process.env.MAX_LOGIN_ATTEMPTS || "5", 10),
    accountLockoutDuration: parseInt(
      process.env.ACCOUNT_LOCKOUT_DURATION || "900000",
      10,
    ),
    rateLimitWindowMs: parseInt(
      process.env.RATE_LIMIT_WINDOW_MS || "900000",
      10,
    ),
    rateLimitMaxRequests: parseInt(
      process.env.RATE_LIMIT_MAX_REQUESTS || "100",
      10,
    ),
    sessionTimeout: parseInt(process.env.SESSION_TIMEOUT || "86400000", 10),
    enable2FA: process.env.ENABLE_2FA === "true",
    auditLogEnabled: process.env.AUDIT_LOG_ENABLED === "true",
    auditLogPath: process.env.AUDIT_LOG_PATH || "./data/audit.log",
    corsAllowedOrigins: (
      process.env.CORS_ALLOWED_ORIGINS || "http://localhost:3000"
    ).split(","),
  };
}

export const config = validateConfig();
