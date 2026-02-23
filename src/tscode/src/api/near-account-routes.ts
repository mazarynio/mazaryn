import express from "express";
import { nearAccountManager } from "../services/near/account.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import {
  transferLimiter,
  exportKeyLimiter,
} from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  CreateNamedAccountRequest,
  CreateSubAccountRequest,
  ImportNearAccountRequest,
  DeleteNearAccountRequest,
} from "../core/near-types.js";

export const nearAccountRoutes = express.Router();

nearAccountRoutes.post(
  "/named",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { signer_wallet_id, account_name, initial_balance_near, method } =
        req.body;

      if (!signer_wallet_id) {
        return res.status(400).json({ error: "signer_wallet_id is required" });
      }

      if (!account_name) {
        return res.status(400).json({ error: "account_name is required" });
      }

      if (!/^[a-z0-9_-]+$/.test(account_name)) {
        return res.status(400).json({
          error:
            "account_name must contain only lowercase letters, numbers, underscores, or hyphens",
        });
      }

      const signerWallet = nearDatabase.getWallet(signer_wallet_id);
      if (!signerWallet) {
        return res.status(404).json({ error: "Signer wallet not found" });
      }

      if (signerWallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near named account creation",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: CreateNamedAccountRequest = {
        user_id: userId,
        signer_wallet_id,
        account_name,
        initial_balance_near,
        method: method === "contract" ? "contract" : "direct",
      };

      const result = await nearAccountManager.createNamedAccount(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_named_account_created",
          account_id: result.account_id,
          wallet_id: result.wallet_id,
          method: request.method,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("NEAR named account creation error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to create account";

      if (
        errorMessage.includes("already exists") ||
        errorMessage.includes("already imported")
      ) {
        return res.status(409).json({ error: errorMessage });
      }

      if (errorMessage.includes("Insufficient")) {
        return res.status(400).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearAccountRoutes.post(
  "/subaccount",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { signer_wallet_id, prefix, initial_balance_near } = req.body;

      if (!signer_wallet_id) {
        return res.status(400).json({ error: "signer_wallet_id is required" });
      }

      if (!prefix) {
        return res.status(400).json({ error: "prefix is required" });
      }

      if (!/^[a-z0-9_-]+$/.test(prefix)) {
        return res.status(400).json({
          error:
            "prefix must contain only lowercase letters, numbers, underscores, or hyphens",
        });
      }

      const signerWallet = nearDatabase.getWallet(signer_wallet_id);
      if (!signerWallet) {
        return res.status(404).json({ error: "Signer wallet not found" });
      }

      if (signerWallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near subaccount creation",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: CreateSubAccountRequest = {
        user_id: userId,
        signer_wallet_id,
        prefix,
        initial_balance_near,
      };

      const result = await nearAccountManager.createSubAccount(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_subaccount_created",
          account_id: result.account_id,
          wallet_id: result.wallet_id,
          parent_account_id: result.parent_account_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("NEAR sub-account creation error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to create sub-account";

      if (errorMessage.includes("already exists")) {
        return res.status(409).json({ error: errorMessage });
      }

      if (errorMessage.includes("Insufficient")) {
        return res.status(400).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearAccountRoutes.post(
  "/import",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { account_id, private_key, account_name } = req.body;

      if (!account_id) {
        return res.status(400).json({ error: "account_id is required" });
      }

      if (!private_key) {
        return res.status(400).json({ error: "private_key is required" });
      }

      const request: ImportNearAccountRequest = {
        user_id: userId,
        account_id,
        private_key,
        account_name,
      };

      const result = await nearAccountManager.importAccount(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_account_imported",
          account_id: result.account_id,
          wallet_id: result.wallet_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("NEAR account import error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to import account";

      if (errorMessage.includes("already imported")) {
        return res.status(409).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearAccountRoutes.delete("/:wallet_id", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { wallet_id } = req.params;
    const { beneficiary_wallet_id } = req.body;

    if (!beneficiary_wallet_id) {
      return res
        .status(400)
        .json({ error: "beneficiary_wallet_id is required" });
    }

    const targetWallet = nearDatabase.getWallet(wallet_id);
    if (!targetWallet) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (targetWallet.user_id !== userId) {
      auditLogger.logUnauthorizedAccess(
        userId,
        req.ip || "unknown",
        `near/accounts/${wallet_id}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: wallet does not belong to user" });
    }

    const beneficiaryWallet = nearDatabase.getWallet(beneficiary_wallet_id);
    if (!beneficiaryWallet) {
      return res.status(404).json({ error: "Beneficiary wallet not found" });
    }

    if (beneficiaryWallet.user_id !== userId) {
      return res.status(403).json({
        error: "Unauthorized: beneficiary wallet does not belong to user",
      });
    }

    const request: DeleteNearAccountRequest = {
      user_id: userId,
      wallet_id,
      beneficiary_wallet_id,
    };

    const result = await nearAccountManager.deleteAccount(request);

    auditLogger.log({
      timestamp: Date.now(),
      event_type: "TRANSFER_COMPLETED" as any,
      user_id: userId,
      success: true,
      details: {
        action: "near_account_deleted",
        deleted_account_id: result.deleted_account_id,
        beneficiary_account_id: result.beneficiary_account_id,
      },
    });

    res.json(result);
  } catch (error) {
    logger.error("NEAR account delete error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Failed to delete account";

    if (errorMessage.includes("same")) {
      return res.status(400).json({ error: errorMessage });
    }

    res.status(500).json({ error: errorMessage });
  }
});

nearAccountRoutes.get("/:wallet_id", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { wallet_id } = req.params;

    const walletInfo = await nearAccountManager.getWalletInfo(wallet_id);
    if (!walletInfo) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (walletInfo.user_id !== userId) {
      auditLogger.logUnauthorizedAccess(
        userId,
        req.ip || "unknown",
        `near/accounts/${wallet_id}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: wallet does not belong to user" });
    }

    res.json(walletInfo);
  } catch (error) {
    logger.error("Get NEAR wallet error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get wallet info",
    });
  }
});

nearAccountRoutes.get(
  "/:wallet_id/balance",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const walletInfo = await nearAccountManager.getWalletInfo(wallet_id);
      if (!walletInfo) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (walletInfo.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/accounts/${wallet_id}/balance`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const balance = await nearAccountManager.getBalance(
        walletInfo.account_id,
      );

      res.json(balance);
    } catch (error) {
      logger.error("Get NEAR balance error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get balance",
      });
    }
  },
);

nearAccountRoutes.get(
  "/:wallet_id/state",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const walletInfo = await nearAccountManager.getWalletInfo(wallet_id);
      if (!walletInfo) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (walletInfo.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/accounts/${wallet_id}/state`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const state = await nearAccountManager.getAccountState(
        walletInfo.account_id,
      );

      res.json(state);
    } catch (error) {
      logger.error("Get NEAR account state error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get account state",
      });
    }
  },
);

nearAccountRoutes.get(
  "/:wallet_id/subaccounts",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const walletInfo = await nearAccountManager.getWalletInfo(wallet_id);
      if (!walletInfo) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (walletInfo.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/accounts/${wallet_id}/subaccounts`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const subAccounts = await nearAccountManager.getSubAccounts(wallet_id);

      res.json({
        parent_account_id: walletInfo.account_id,
        total: subAccounts.length,
        subaccounts: subAccounts,
      });
    } catch (error) {
      logger.error("Get NEAR sub-accounts error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get sub-accounts",
      });
    }
  },
);

nearAccountRoutes.get(
  "/:wallet_id/export-key",
  authenticate,
  exportKeyLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const walletInfo = await nearAccountManager.getWalletInfo(wallet_id);
      if (!walletInfo) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (walletInfo.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/accounts/${wallet_id}/export-key`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      auditLogger.logSuspiciousActivity(
        userId,
        req.ip || "unknown",
        "NEAR private key export",
        { wallet_id, account_id: walletInfo.account_id },
      );

      const privateKey = await nearAccountManager.exportPrivateKey(wallet_id);

      res.json({
        wallet_id,
        account_id: walletInfo.account_id,
        private_key: privateKey,
        warning: "Keep this private key secure. Never share it with anyone.",
      });
    } catch (error) {
      logger.error("Export NEAR private key error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to export private key",
      });
    }
  },
);

nearAccountRoutes.get("/user/:user_id", authenticate, async (req: any, res) => {
  try {
    const requestingUserId = req.user.user_id;
    const targetUserId = req.params.user_id;

    if (requestingUserId !== targetUserId) {
      auditLogger.logUnauthorizedAccess(
        requestingUserId,
        req.ip || "unknown",
        `near/accounts/user/${targetUserId}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: cannot access other user wallets" });
    }

    const wallets = await nearAccountManager.getUserWallets(targetUserId);

    res.json({
      user_id: targetUserId,
      total: wallets.length,
      wallets,
    });
  } catch (error) {
    logger.error("Get user NEAR wallets error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get user wallets",
    });
  }
});
