import express from "express";
import { nearStateManager } from "../services/near/state.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";

export const nearStateRoutes = express.Router();

nearStateRoutes.get(
  "/account/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;

      const state =
        await nearStateManager.getAccountStateByAccountId(account_id);
      res.json(state);
    } catch (error) {
      logger.error("Get NEAR account state error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to get account state";

      if (errorMessage.includes("does not exist")) {
        return res.status(404).json({ error: "Account not found on-chain" });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearStateRoutes.get(
  "/wallet/:wallet_id/state",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/state/wallet/${wallet_id}/state`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const state = await nearStateManager.getAccountStateByWalletId(
        wallet_id,
        userId,
      );
      res.json(state);
    } catch (error) {
      logger.error("Get wallet state error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get wallet state",
      });
    }
  },
);

nearStateRoutes.get(
  "/wallet/:wallet_id/balance/near",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/state/wallet/${wallet_id}/balance/near`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const balance = await nearStateManager.getNearBalance(wallet.account_id);
      res.json(balance);
    } catch (error) {
      logger.error("Get NEAR balance error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get balance",
      });
    }
  },
);

nearStateRoutes.get(
  "/balance/:account_id/near",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const balance = await nearStateManager.getNearBalance(account_id);
      res.json(balance);
    } catch (error) {
      logger.error("Get NEAR balance error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get balance",
      });
    }
  },
);

nearStateRoutes.post(
  "/wallet/:wallet_id/balance/tokens",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { custom_tokens } = req.body;

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/state/wallet/${wallet_id}/balance/tokens`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const balances = await nearStateManager.getMultiTokenBalance(
        wallet.account_id,
        custom_tokens,
      );

      res.json(balances);
    } catch (error) {
      logger.error("Get multi-token balances error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get token balances",
      });
    }
  },
);

nearStateRoutes.post(
  "/balance/:account_id/tokens",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const { custom_tokens } = req.body;

      const balances = await nearStateManager.getMultiTokenBalance(
        account_id,
        custom_tokens,
      );

      res.json(balances);
    } catch (error) {
      logger.error("Get multi-token balances error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get token balances",
      });
    }
  },
);

nearStateRoutes.get("/epoch-prices", generalLimiter, async (req: any, res) => {
  try {
    const prices = await nearStateManager.getEpochPrices();
    res.json(prices);
  } catch (error) {
    logger.error("Get epoch prices error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get epoch prices",
    });
  }
});
