import express from "express";
import { transactionHistoryManager } from "../services/solana/transaction-history.js";
import { walletManager } from "../services/solana/wallet.js";
import { authenticate } from "../middleware/auth.js";
import { generalLimiter } from "../middleware/rate-limiter.js";
import { logger } from "../core/logger.js";

export const transactionRoutes = express.Router();

transactionRoutes.post(
  "/history",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { public_key, wallet_id, limit, before, until } = req.body;

      if (!public_key && !wallet_id) {
        return res
          .status(400)
          .json({ error: "public_key or wallet_id is required" });
      }

      let publicKey = public_key;

      if (wallet_id && !public_key) {
        const walletInfo = await walletManager.getWalletInfo(wallet_id);
        if (!walletInfo) {
          return res.status(404).json({ error: "Wallet not found" });
        }

        if (walletInfo.user_id !== userId) {
          return res
            .status(403)
            .json({ error: "Unauthorized: wallet does not belong to user" });
        }

        publicKey = walletInfo.public_key;
      }

      if (!publicKey) {
        return res
          .status(400)
          .json({ error: "Could not determine public key" });
      }

      const result = await transactionHistoryManager.getTransactionHistory({
        public_key: publicKey,
        limit: limit ? parseInt(limit, 10) : 20,
        before,
        until,
      });

      res.json(result);
    } catch (error) {
      logger.error("Transaction history error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get transaction history",
      });
    }
  },
);

transactionRoutes.get(
  "/history/:public_key",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const publicKey = req.params.public_key;
      const limit = req.query.limit
        ? parseInt(req.query.limit as string, 10)
        : 20;
      const before = req.query.before as string | undefined;
      const until = req.query.until as string | undefined;

      if (!publicKey) {
        return res.status(400).json({ error: "public_key is required" });
      }

      const result = await transactionHistoryManager.getTransactionHistory({
        public_key: publicKey,
        limit,
        before,
        until,
      });

      res.json(result);
    } catch (error) {
      logger.error("Transaction history error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get transaction history",
      });
    }
  },
);

transactionRoutes.get(
  "/stats/:public_key",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const publicKey = req.params.public_key;

      if (!publicKey) {
        return res.status(400).json({ error: "public_key is required" });
      }

      const result =
        await transactionHistoryManager.getTransactionStats(publicKey);
      res.json(result);
    } catch (error) {
      logger.error("Transaction stats error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get transaction stats",
      });
    }
  },
);

transactionRoutes.post(
  "/stats",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { public_key, wallet_id } = req.body;

      if (!public_key && !wallet_id) {
        return res
          .status(400)
          .json({ error: "public_key or wallet_id is required" });
      }

      let publicKey = public_key;

      if (wallet_id && !public_key) {
        const walletInfo = await walletManager.getWalletInfo(wallet_id);
        if (!walletInfo) {
          return res.status(404).json({ error: "Wallet not found" });
        }

        if (walletInfo.user_id !== userId) {
          return res
            .status(403)
            .json({ error: "Unauthorized: wallet does not belong to user" });
        }

        publicKey = walletInfo.public_key;
      }

      if (!publicKey) {
        return res
          .status(400)
          .json({ error: "Could not determine public key" });
      }

      const result =
        await transactionHistoryManager.getTransactionStats(publicKey);
      res.json(result);
    } catch (error) {
      logger.error("Transaction stats error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get transaction stats",
      });
    }
  },
);

transactionRoutes.post(
  "/filter",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { public_key, wallet_id, limit, before, until, type, status } =
        req.body;

      if (!public_key && !wallet_id) {
        return res
          .status(400)
          .json({ error: "public_key or wallet_id is required" });
      }

      let publicKey = public_key;

      if (wallet_id && !public_key) {
        const walletInfo = await walletManager.getWalletInfo(wallet_id);
        if (!walletInfo) {
          return res.status(404).json({ error: "Wallet not found" });
        }

        if (walletInfo.user_id !== userId) {
          return res
            .status(403)
            .json({ error: "Unauthorized: wallet does not belong to user" });
        }

        publicKey = walletInfo.public_key;
      }

      if (!publicKey) {
        return res
          .status(400)
          .json({ error: "Could not determine public key" });
      }

      const result = await transactionHistoryManager.getTransactionHistory({
        public_key: publicKey,
        limit: limit ? parseInt(limit, 10) : 20,
        before,
        until,
      });

      let filtered = result.transactions;

      if (type) {
        filtered = filtered.filter((tx) => tx.type === type);
      }

      if (status) {
        filtered = filtered.filter((tx) => tx.status === status);
      }

      res.json({
        ...result,
        transactions: filtered,
        total: filtered.length,
      });
    } catch (error) {
      logger.error("Transaction filter error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to filter transactions",
      });
    }
  },
);

transactionRoutes.get("/types", authenticate, async (req: any, res) => {
  try {
    res.json({
      types: [
        { type: "sol_transfer", description: "SOL transfer between wallets" },
        { type: "token_transfer", description: "SPL token transfer" },
        { type: "token_mint", description: "New tokens minted" },
        { type: "token_burn", description: "Tokens burned" },
        { type: "token_account_create", description: "Token account creation" },
        { type: "token_operation", description: "Generic token operation" },
        { type: "nft_operation", description: "NFT related operation" },
        { type: "token_swap", description: "Token swap via DEX" },
        { type: "staking", description: "Staking operation" },
        { type: "vote", description: "Validator vote transaction" },
        { type: "unknown", description: "Unrecognized transaction type" },
      ],
    });
  } catch (error) {
    logger.error("Transaction types error:", error);
    res.status(500).json({
      error:
        error instanceof Error
          ? error.message
          : "Failed to get transaction types",
    });
  }
});
