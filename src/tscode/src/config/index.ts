import dotenv from "dotenv";
import path from "path";

dotenv.config();

interface Config {
  nodeEnv: string;
  port: number;
  solanaRpcUrl: string;
  encryptionKey: string;
  jwtSecret: string;
  databasePath: string;
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

  return {
    nodeEnv: process.env.NODE_ENV || "development",
    port: parseInt(process.env.PORT || "3020", 10),
    solanaRpcUrl: process.env.SOLANA_RPC_URL || "https://api.devnet.solana.com",
    encryptionKey,
    jwtSecret,
    databasePath: process.env.DATABASE_PATH || "./data/wallets.db",
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
