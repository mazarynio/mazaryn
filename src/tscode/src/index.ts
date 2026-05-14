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
import { aptosAccountRoutes } from "./api/aptos-account-routes.js";
import { aptosTransactionRoutes } from "./api/aptos-transaction-routes.js";
import { aptosNFTRoutes } from "./api/aptos-nft-routes.js";
import { aptosLedgerRoutes } from "./api/aptos-ledger-routes.js";
import { aptosTokenRoutes } from "./api/aptos-token-routes.js";
import { aptosStakingRoutes } from "./api/aptos-staking-routes.js";
import suiRouter from "./api/sui-routes.js";
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

app.use("/aptos/accounts", aptosAccountRoutes);
app.use("/aptos/transactions", aptosTransactionRoutes);
app.use("/aptos/nft", aptosNFTRoutes);
app.use("/aptos/tokens", aptosTokenRoutes);
app.use("/aptos/staking", aptosStakingRoutes);
app.use("/aptos", aptosLedgerRoutes);

app.use("/sui", suiRouter);

app.get("/health", (_req, res) => {
  res.json({
    status: "ok",
    timestamp: Date.now(),
    environment: config.nodeEnv,
    service: "solana-near-aptos-sui-wallet",
    version: "9.0.0",
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
      aptos: {
        wallet_create: true,
        wallet_import: true,
        wallet_delete: true,
        apt_balance: true,
        fungible_asset_balance: true,
        apt_transfer: true,
        fungible_asset_transfer: true,
        coin_transfer: true,
        coin_balance: true,
        coin_mint: true,
        fa_metadata: true,
        fa_activities: true,
        view_function: true,
        nft_collections: true,
        nft_mint: true,
        nft_transfer: true,
        nft_burn: true,
        owned_tokens: true,
        transaction_queries: true,
        account_transactions: true,
        gas_estimation: true,
        ledger_info: true,
        block_queries: true,
        staking_add: true,
        staking_unlock: true,
        staking_withdraw: true,
        staking_reactivate: true,
        staking_info: true,
        delegation_pools: true,
        staking_activities: true,
      },
      sui: {
        wallet_create: true,
        wallet_import_private_key: true,
        wallet_import_mnemonic: true,
        wallet_delete: true,
        multi_scheme_keypairs: true,
        ed25519: true,
        secp256k1: true,
        secp256r1: true,
        sui_balance: true,
        all_coin_balances: true,
        coin_objects: true,
        owned_objects: true,
        sui_transfer: true,
        batch_transfer: true,
        coin_split: true,
        coin_merge: true,
        coin_metadata: true,
        move_call: true,
        publish_package: true,
        upgrade_package: true,
        transaction_build: true,
        transaction_execute: true,
        transaction_simulate: true,
        transaction_wait: true,
        transaction_history_local: true,
        nft_mint: true,
        nft_transfer: true,
        nft_query: true,
        dynamic_fields: true,
        object_query: true,
        batch_object_query: true,
        staking: true,
        unstaking: true,
        multi_coin_staking: true,
        validator_query: true,
        staking_rewards: true,
        epoch_info: true,
        system_state: true,
        sign_message: true,
        verify_signature: true,
        verify_transaction_signature: true,
        multisig_create: true,
        multisig_combine_execute: true,
        name_service_lookup: true,
        name_service_reverse: true,
        move_package_inspect: true,
        move_function_inspect: true,
        move_type_inspect: true,
        package_versions: true,
        checkpoint_query: true,
        reference_gas_price: true,
        chain_info: true,
        faucet_devnet_testnet: true,
        grpc_transport: true,
      },
    },
    networks: {
      solana: config.solanaRpcUrl,
      near: {
        network: config.nearNetwork,
        rpc: config.nearRpcUrl,
        failover_endpoints: config.nearFallbackRpcUrls.length,
      },
      aptos: {
        network: config.aptosNetwork,
        node_url: config.aptosNodeUrl,
      },
      sui: {
        mainnet: "https://fullnode.mainnet.sui.io:443",
        testnet: "https://fullnode.testnet.sui.io:443",
        devnet: "https://fullnode.devnet.sui.io:443",
        transport: "gRPC",
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
  logger.info("═══════════════════════════════════════════════════");
  logger.info("  Solana + NEAR + Aptos + Sui Wallet Service");
  logger.info("═══════════════════════════════════════════════════");
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
  logger.info(`  Aptos Network: ${config.aptosNetwork}`);
  logger.info(`  Aptos Node   : ${config.aptosNodeUrl}`);
  logger.info("  Sui Networks : mainnet | testnet | devnet | localnet");
  logger.info("  Sui Transport: gRPC (SuiGrpcClient)");
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
  logger.info("───────────────────────────────────────────────────");
  logger.info("  Aptos routes:");
  logger.info(
    "    /aptos/accounts/**       create, import, delete, balance, FA balance",
  );
  logger.info(
    "    /aptos/transactions/**   APT transfer, FA transfer, gas estimate, tx queries",
  );
  logger.info(
    "    /aptos/nft/**            collections, mint, transfer, burn, owned tokens",
  );
  logger.info(
    "    /aptos/tokens/**         coin balance, FA balance, FA metadata, transfer coin,",
  );
  logger.info(
    "                             transfer FA, mint coin, view function, FA activities",
  );
  logger.info(
    "    /aptos/staking/**        add stake, unlock, withdraw, reactivate,",
  );
  logger.info(
    "                             stake info, delegation pools, staking activities",
  );
  logger.info("    /aptos/ledger            ledger info");
  logger.info("    /aptos/blocks/**         block by height");
  logger.info("───────────────────────────────────────────────────");
  logger.info("  Sui routes (all via gRPC):");
  logger.info(
    "    /sui/wallets/**          create, import (key/mnemonic), delete,",
  );
  logger.info(
    "                             info, label, primary, Ed25519/k1/r1",
  );
  logger.info("    /sui/wallets/:id/balance[/:coinType]");
  logger.info("    /sui/wallets/:id/balances    all coin balances");
  logger.info("    /sui/wallets/:id/coins       owned coin objects");
  logger.info("    /sui/wallets/:id/objects     owned Move objects");
  logger.info("    /sui/wallets/:id/sign        sign personal message");
  logger.info("    /sui/transfer                SUI transfer");
  logger.info(
    "    /sui/transfer/batch          batch multi-recipient transfer",
  );
  logger.info("    /sui/coins/split|merge       coin management");
  logger.info("    /sui/coins/info/:coinType    coin metadata & treasury");
  logger.info("    /sui/transactions/:digest    get transaction");
  logger.info("    /sui/transactions/batch      batch fetch");
  logger.info("    /sui/transactions/execute    submit raw tx + signatures");
  logger.info("    /sui/transactions/simulate   dry-run with balance changes");
  logger.info("    /sui/transactions/wait       wait for finality");
  logger.info("    /sui/transactions/build      build PTB bytes");
  logger.info("    /sui/history/:walletId       local tx history (paginated)");
  logger.info("    /sui/history/user/:userId    user-level history");
  logger.info("    /sui/history/digest/:digest  lookup by digest");
  logger.info("    /sui/move/call               execute Move function");
  logger.info("    /sui/move/publish            publish Move package");
  logger.info("    /sui/move/upgrade            upgrade Move package");
  logger.info("    /sui/move/package/:id        inspect package modules");
  logger.info("    /sui/move/package/:id/versions");
  logger.info("    /sui/move/package/:id/module/:m/function/:f");
  logger.info("    /sui/move/package/:id/module/:m/type/:t");
  logger.info("    /sui/staking/validators      active validator list + APY");
  logger.info("    /sui/staking/validators/:addr");
  logger.info("    /sui/staking/epoch           current epoch + system state");
  logger.info("    /sui/staking/stake           add stake to validator");
  logger.info("    /sui/staking/stake/multi-coin");
  logger.info("    /sui/staking/unstake         withdraw staked SUI");
  logger.info("    /sui/staking/staked/:owner   staked SUI positions");
  logger.info("    /sui/staking/rewards/:owner  estimated rewards");
  logger.info("    /sui/staking/history/:walletId");
  logger.info("    /sui/objects/:objectId       fetch object");
  logger.info("    /sui/objects/batch           fetch multiple objects");
  logger.info("    /sui/objects/:id/dynamic-fields");
  logger.info("    /sui/nft/mint                mint NFT via Move call");
  logger.info("    /sui/nft/transfer            transfer NFT object");
  logger.info("    /sui/nft/:owner              list owned NFTs");
  logger.info("    /sui/verify/signature        verify personal message sig");
  logger.info("    /sui/verify/transaction      verify transaction sig");
  logger.info("    /sui/multisig/create         derive multisig address");
  logger.info(
    "    /sui/multisig/combine        combine partial sigs + execute",
  );
  logger.info("    /sui/names/lookup            SuiNS forward lookup");
  logger.info("    /sui/names/reverse/:address  SuiNS reverse lookup");
  logger.info("    /sui/chain/info              service info + chain ID");
  logger.info("    /sui/chain/gas-price         reference gas price");
  logger.info("    /sui/chain/checkpoint        checkpoint by seq/digest");
  logger.info("    /sui/chain/epoch             epoch data");
  logger.info("    /sui/faucet                  devnet/testnet faucet");
  logger.info("═══════════════════════════════════════════════════");
  logger.info("  System ready.");
  logger.info("═══════════════════════════════════════════════════");
});
