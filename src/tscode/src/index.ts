import express from "express";
import { walletRoutes } from "./api/routes.js";
import { authRoutes } from "./api/auth-routes.js";
import { twoFARoutes } from "./api/two-fa-routes.js";
import { airdropRoutes } from "./api/airdrop-routes.js";
import { transactionRoutes } from "./api/transaction-routes.js";
import { stakingRoutes } from "./api/staking-routes.js";
import { logger } from "./core/logger.js";
import { config } from "./config/index.js";
import { setupSecurityHeaders } from "./middleware/security-headers.js";
import { generalLimiter } from "./middleware/rate-limiter.js";
import { auditLogger } from "./services/security/audit-logger.js";

const app = express();

setupSecurityHeaders(app);

app.use(express.json({ limit: "10mb" }));
app.use(express.urlencoded({ extended: true, limit: "10mb" }));

app.set("trust proxy", true);

app.use((req, res, next) => {
  const startTime = Date.now();

  res.on("finish", () => {
    const duration = Date.now() - startTime;
    const ip = req.ip || req.socket.remoteAddress || "unknown";

    if (
      config.auditLogEnabled &&
      (res.statusCode >= 400 || req.path.includes("/auth/"))
    ) {
      logger.info(
        `${req.method} ${req.path} - ${res.statusCode} - ${duration}ms - ${ip}`,
      );
    }
  });

  next();
});

app.use(generalLimiter);

app.use("/auth", authRoutes);
app.use("/2fa", twoFARoutes);
app.use("/wallet", walletRoutes);
app.use("/airdrop", airdropRoutes);
app.use("/transactions", transactionRoutes);
app.use("/staking", stakingRoutes);

app.get("/health", (req, res) => {
  res.json({
    status: "ok",
    timestamp: Date.now(),
    environment: config.nodeEnv,
    service: "solana-wallet",
    version: "5.0.0",
    security_features: {
      encryption: "AES-256-GCM",
      password_hashing: "PBKDF2",
      two_factor_auth: config.enable2FA,
      rate_limiting: true,
      audit_logging: config.auditLogEnabled,
      session_management: true,
    },
    features: {
      wallet_operations: true,
      spl_tokens: true,
      nft_support: true,
      airdrop_system: true,
      transaction_history: true,
      staking: true,
    },
  });
});

app.get("/security-info", (req, res) => {
  res.json({
    features: [
      "AES-256-GCM encryption for private keys",
      "PBKDF2 password hashing with salt",
      "JWT-based authentication with session tracking",
      "Two-factor authentication (TOTP)",
      "Rate limiting on all endpoints",
      "Account lockout after failed attempts",
      "IP blocking for suspicious activity",
      "Comprehensive audit logging",
      "Session management and revocation",
      "Security headers (Helmet, CORS)",
      "Password complexity requirements",
    ],
    recommendations: [
      "Enable 2FA for maximum security",
      "Use strong, unique passwords",
      "Review active sessions regularly",
      "Monitor security notifications",
      "Keep backup codes in a secure location",
    ],
  });
});

app.get("/audit/summary", (req, res) => {
  try {
    if (!config.auditLogEnabled) {
      return res.status(503).json({ error: "Audit logging is disabled" });
    }

    const last24Hours = Date.now() - 24 * 60 * 60 * 1000;
    const recentLogs = auditLogger.queryLogs({ startTime: last24Hours });

    const summary = {
      total_events: recentLogs.length,
      successful_logins: recentLogs.filter(
        (l) => l.event_type === "USER_LOGIN_SUCCESS",
      ).length,
      failed_logins: recentLogs.filter(
        (l) => l.event_type === "USER_LOGIN_FAILED",
      ).length,
      wallet_creations: recentLogs.filter(
        (l) => l.event_type === "WALLET_CREATED",
      ).length,
      transfers: recentLogs.filter((l) => l.event_type === "TRANSFER_COMPLETED")
        .length,
      suspicious_activities: recentLogs.filter(
        (l) => l.event_type === "SUSPICIOUS_ACTIVITY",
      ).length,
      rate_limit_exceeded: recentLogs.filter(
        (l) => l.event_type === "RATE_LIMIT_EXCEEDED",
      ).length,
      unauthorized_access: recentLogs.filter(
        (l) => l.event_type === "UNAUTHORIZED_ACCESS",
      ).length,
    };

    res.json({
      period: "last_24_hours",
      summary,
      timestamp: Date.now(),
    });
  } catch (error) {
    logger.error("Audit summary error:", error);
    res.status(500).json({ error: "Failed to generate audit summary" });
  }
});

app.use((req, res) => {
  const ip = req.ip || req.socket.remoteAddress || "unknown";
  auditLogger.logSuspiciousActivity(
    undefined,
    ip,
    "Access to non-existent endpoint",
    { path: req.path, method: req.method },
  );

  res.status(404).json({
    error: "Endpoint not found",
    path: req.path,
  });
});

app.use(
  (
    err: any,
    req: express.Request,
    res: express.Response,
    next: express.NextFunction,
  ) => {
    logger.error("Unhandled error:", err);

    const ip = req.ip || req.socket.remoteAddress || "unknown";
    auditLogger.logSuspiciousActivity(undefined, ip, "Unhandled server error", {
      error: err.message,
      path: req.path,
      method: req.method,
    });

    res.status(500).json({
      error: "Internal server error",
      message:
        config.nodeEnv === "development"
          ? err.message
          : "An unexpected error occurred",
    });
  },
);

app.listen(config.port, () => {
  logger.info(`Solana Wallet Service running on port ${config.port}`);
  logger.info(`Environment: ${config.nodeEnv}`);
  logger.info(`RPC URL: ${config.solanaRpcUrl}`);
  logger.info(`Security Features:`);
  logger.info(`  - Encryption: AES-256-GCM`);
  logger.info(`  - 2FA: ${config.enable2FA ? "Enabled" : "Disabled"}`);
  logger.info(`  - Rate Limiting: Enabled`);
  logger.info(
    `  - Audit Logging: ${config.auditLogEnabled ? "Enabled" : "Disabled"}`,
  );
  logger.info(`  - Session Management: Enabled`);
  logger.info(`  - Account Lockout: ${config.maxLoginAttempts} attempts`);
  logger.info(`Wallet Features:`);
  logger.info(`  - Wallet Operations: Enabled`);
  logger.info(`  - SPL Token Support: Enabled`);
  logger.info(`  - NFT Support: Enabled`);
  logger.info(`  - Airdrop System: Enabled`);
  logger.info(`  - Transaction History: Enabled`);
  logger.info(`  - Staking: Enabled`);
  logger.info(`System Ready - Enterprise-Grade Security Active`);
});
