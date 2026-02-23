import express from "express";
import { walletRoutes } from "./api/routes.js";
import { authRoutes } from "./api/auth-routes.js";
import { twoFARoutes } from "./api/two-fa-routes.js";
import { airdropRoutes } from "./api/airdrop-routes.js";
import { transactionRoutes } from "./api/transaction-routes.js";
import { stakingRoutes } from "./api/staking-routes.js";
import { nearAccountRoutes } from "./api/near-account-routes.js";
import { nearTokenRoutes } from "./api/near-token-routes.js";
import { nearStateRoutes } from "./api/near-state-routes.js";
import { nearKeyRoutes } from "./api/near-keys-routes.js";
import { nearTransactionRoutes } from "./api/near-transaction-routes.js";
import { nearAdvancedRoutes } from "./api/near-advanced-routes.js";
import { nearContractRoutes } from "./api/near-contract-routes.js";
import { nearAuthRoutes } from "./api/near-auth-routes.js";
import { nearStakingRoutes } from "./api/near-staking-routes.js";
import { nearSocialRoutes } from "./api/near-social-routes.js";
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

app.use("/near/accounts", nearAccountRoutes);
app.use("/near/tokens", nearTokenRoutes);
app.use("/near/state", nearStateRoutes);
app.use("/near/keys", nearKeyRoutes);
app.use("/near/tx", nearTransactionRoutes);
app.use("/near/advanced", nearAdvancedRoutes);
app.use("/near/contracts", nearContractRoutes);
app.use("/near/auth", nearAuthRoutes);
app.use("/near/staking", nearStakingRoutes);
app.use("/near/social", nearSocialRoutes);

app.get("/health", (_req, res) => {
  res.json({
    status: "ok",
    timestamp: Date.now(),
    environment: config.nodeEnv,
    service: "solana-near-wallet",
    version: "6.6.0",
    security_features: {
      encryption: "AES-256-GCM",
      password_hashing: "PBKDF2",
      two_factor_auth: config.enable2FA,
      rate_limiting: true,
      audit_logging: config.auditLogEnabled,
      session_management: true,
    },
    features: {
      solana: {
        wallet_operations: true,
        spl_tokens: true,
        nft_support: true,
        airdrop_system: true,
        transaction_history: true,
        staking: true,
      },
      near: {
        accounts: true,
        tokens: true,
        state: true,
        keys: true,
        transactions: true,
        batch_actions: true,
        contract_calls: true,
        view_calls: true,
        sign_and_broadcast: true,
        seed_phrase: true,
        function_call_keys: true,
        epoch_prices: true,
        meta_transactions: true,
        implicit_accounts: true,
        multi_key_signing: true,
        contract_deployment: true,
        global_contracts: true,
        nep413_verification: true,
        staking: true,
        validator_queries: true,
        social_db: true,
        social_profile: true,
        social_follow: true,
        social_posts: true,
        social_likes: true,
        social_comments: true,
        social_feed: true,
        mazaryn_posts: true,
        mazaryn_likes: true,
        mazaryn_comments: true,
        mazaryn_follow: true,
        mazaryn_tips: true,
        mazaryn_feed: true,
      },
    },
    networks: {
      solana: config.solanaRpcUrl,
      near: {
        network: config.nearNetwork,
        rpc: config.nearRpcUrl,
        failover_endpoints: config.nearFallbackRpcUrls.length,
      },
    },
  });
});

app.get("/security-info", (_req, res) => {
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
      "NEP-413 message verification for NEAR wallet login",
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

app.get("/audit/summary", (_req, res) => {
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
    _next: express.NextFunction,
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
  logger.info(`  Port        : ${config.port}`);
  logger.info(`  Environment : ${config.nodeEnv}`);
  logger.info("───────────────────────────────────────────────────");
  logger.info(`  Solana RPC  : ${config.solanaRpcUrl}`);
  logger.info(`  NEAR Network: ${config.nearNetwork}`);
  logger.info(`  NEAR RPC    : ${config.nearRpcUrl}`);
  if (config.nearFallbackRpcUrls.length > 0) {
    config.nearFallbackRpcUrls.forEach((url, i) =>
      logger.info(`  NEAR Fallback [${i + 1}]: ${url}`),
    );
  }
  logger.info("───────────────────────────────────────────────────");
  logger.info("  Security:");
  logger.info(`    Encryption      : AES-256-GCM`);
  logger.info(
    `    2FA             : ${config.enable2FA ? "Enabled" : "Disabled"}`,
  );
  logger.info(`    Rate Limiting   : Enabled`);
  logger.info(
    `    Audit Logging   : ${config.auditLogEnabled ? "Enabled" : "Disabled"}`,
  );
  logger.info(`    Session Mgmt    : Enabled`);
  logger.info(
    `    Account Lockout : after ${config.maxLoginAttempts} failed attempts`,
  );
  logger.info("───────────────────────────────────────────────────");
  logger.info("  Solana routes:");
  logger.info("    /auth/**");
  logger.info("    /2fa/**");
  logger.info("    /wallet/**");
  logger.info("    /airdrop/**");
  logger.info("    /transactions/**");
  logger.info("    /staking/**");
  logger.info("───────────────────────────────────────────────────");
  logger.info("  NEAR routes:");
  logger.info("    /near/accounts/**    named, sub-account, import, delete");
  logger.info("    /near/tokens/**      NEAR, USDT, custom FTs");
  logger.info("    /near/state/**       account state, balances, epoch prices");
  logger.info("    /near/keys/**        FAK, FCK, seed phrase");
  logger.info("    /near/tx/**          batch, call, view, sign, broadcast");
  logger.info("    /near/advanced/**    meta-tx, implicit, multi-key");
  logger.info("    /near/contracts/**   deploy, global deploy, use global");
  logger.info("    /near/auth/**        NEP-413 message verification");
  logger.info(
    "    /near/staking/**     stake, unstake, withdraw, validator info",
  );
  logger.info(
    "    /near/social/**      SocialDB + Mazaryn posts, likes, follows, tips",
  );
  logger.info("═══════════════════════════════════════════════════");
  logger.info("  System ready.");
  logger.info("═══════════════════════════════════════════════════");
});
