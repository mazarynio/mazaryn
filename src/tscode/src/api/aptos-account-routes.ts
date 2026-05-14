import express from "express";
import { aptosAccountManager } from "../services/aptos/account.js";
import { aptosDatabase } from "../services/database/aptos-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  CreateAptosAccountRequest,
  ImportAptosAccountRequest,
  DeleteAptosAccountRequest,
} from "../core/aptos-types.js";

export const aptosAccountRoutes = express.Router();

aptosAccountRoutes.post(
  "/create",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { label } = req.body;

      const request: CreateAptosAccountRequest = { user_id: userId, label };
      const result = await aptosAccountManager.createAccount(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_account_created",
          account_address: result.account_address,
          wallet_id: result.wallet_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Aptos account creation error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to create account",
      });
    }
  },
);

aptosAccountRoutes.post(
  "/import",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { private_key, label } = req.body;

      if (!private_key) {
        return res.status(400).json({ error: "private_key is required" });
      }

      const request: ImportAptosAccountRequest = {
        user_id: userId,
        private_key,
        label,
      };
      const result = await aptosAccountManager.importAccount(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_account_imported",
          account_address: result.account_address,
          wallet_id: result.wallet_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Aptos account import error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to import account";

      if (errorMessage.includes("already imported")) {
        return res.status(409).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

aptosAccountRoutes.delete("/:address", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { address } = req.params;

    const wallet = aptosDatabase.getWalletByAddress(address);
    if (!wallet) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (wallet.user_id !== userId) {
      auditLogger.logUnauthorizedAccess(
        userId,
        req.ip || "unknown",
        `aptos/accounts/${address}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: wallet does not belong to user" });
    }

    const request: DeleteAptosAccountRequest = {
      user_id: userId,
      wallet_id: wallet.wallet_id,
    };
    const deleted = aptosAccountManager.deleteAccount(request);

    if (!deleted) {
      return res.status(500).json({ error: "Failed to delete wallet" });
    }

    auditLogger.log({
      timestamp: Date.now(),
      event_type: "WALLET_DELETED",
      user_id: userId,
      success: true,
      details: { action: "aptos_account_deleted", account_address: address },
    });

    res.json({ success: true, account_address: address });
  } catch (error) {
    logger.error("Aptos account delete error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to delete account",
    });
  }
});

aptosAccountRoutes.get(
  "/:address/balance",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { address } = req.params;
      const { network } = req.query;

      const wallet = aptosDatabase.getWalletByAddress(address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `aptos/accounts/${address}/balance`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const balance = await aptosAccountManager.getBalance(
        address,
        network as string,
      );
      res.json(balance);
    } catch (error) {
      logger.error("Get Aptos balance error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get balance",
      });
    }
  },
);

aptosAccountRoutes.get(
  "/:address/fa-balance/:metadata_address",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { address, metadata_address } = req.params;
      const { network } = req.query;

      const wallet = aptosDatabase.getWalletByAddress(address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `aptos/accounts/${address}/fa-balance/${metadata_address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const balance = await aptosAccountManager.getFABalance(
        address,
        metadata_address,
        network as string,
      );
      res.json(balance);
    } catch (error) {
      logger.error("Get Aptos FA balance error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get FA balance",
      });
    }
  },
);

aptosAccountRoutes.get(
  "/user/:user_id",
  authenticate,
  async (req: any, res) => {
    try {
      const requestingUserId = req.user.user_id;
      const targetUserId = req.params.user_id;

      if (requestingUserId !== targetUserId) {
        auditLogger.logUnauthorizedAccess(
          requestingUserId,
          req.ip || "unknown",
          `aptos/accounts/user/${targetUserId}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: cannot access other user wallets" });
      }

      const wallets = aptosAccountManager.getUserWallets(targetUserId);
      res.json({ user_id: targetUserId, total: wallets.length, wallets });
    } catch (error) {
      logger.error("Get user Aptos wallets error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get user wallets",
      });
    }
  },
);

aptosAccountRoutes.get("/:address", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { address } = req.params;

    const wallet = aptosDatabase.getWalletByAddress(address);
    if (!wallet) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (wallet.user_id !== userId) {
      auditLogger.logUnauthorizedAccess(
        userId,
        req.ip || "unknown",
        `aptos/accounts/${address}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: wallet does not belong to user" });
    }

    const info = aptosAccountManager.getWalletInfo(wallet.wallet_id);
    res.json(info);
  } catch (error) {
    logger.error("Get Aptos wallet info error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get wallet info",
    });
  }
});
