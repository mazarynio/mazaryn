import express from "express";
import { aptosTransactionManager } from "../services/aptos/transaction.js";
import { aptosDatabase } from "../services/database/aptos-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type { AptosTransferRequest } from "../core/aptos-types.js";

export const aptosTransactionRoutes = express.Router();

aptosTransactionRoutes.post(
  "/transfer",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        sender_address,
        recipient_address,
        amount_octas,
        network,
        options,
      } = req.body;

      if (!sender_address) {
        return res.status(400).json({ error: "sender_address is required" });
      }
      if (!recipient_address) {
        return res.status(400).json({ error: "recipient_address is required" });
      }
      if (amount_octas === undefined || amount_octas <= 0) {
        return res
          .status(400)
          .json({ error: "amount_octas must be a positive number" });
      }

      const wallet = aptosDatabase.getWalletByAddress(sender_address);
      if (!wallet) {
        return res.status(404).json({ error: "Sender wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/transactions/transfer",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosTransferRequest = {
        sender_address,
        recipient_address,
        amount_octas,
        network,
        options,
      };

      const result = await aptosTransactionManager.transferAPT(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_apt_transfer",
          sender_address,
          recipient_address,
          amount_octas,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos APT transfer error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to transfer APT",
      });
    }
  },
);

aptosTransactionRoutes.get(
  "/estimate-gas",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { network } = req.query;
      const result = await aptosTransactionManager.estimateGas(
        network as string,
      );
      res.json(result);
    } catch (error) {
      logger.error("Aptos gas estimate error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to estimate gas",
      });
    }
  },
);

aptosTransactionRoutes.get(
  "/history/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const limit = req.query.limit
        ? parseInt(req.query.limit as string, 10)
        : 20;
      const offset = req.query.offset
        ? parseInt(req.query.offset as string, 10)
        : 0;

      if (isNaN(limit) || limit <= 0 || limit > 100) {
        return res
          .status(400)
          .json({ error: "limit must be between 1 and 100" });
      }
      if (isNaN(offset) || offset < 0) {
        return res
          .status(400)
          .json({ error: "offset must be a non-negative integer" });
      }

      const result = await aptosTransactionManager.getLocalTransactionHistory(
        wallet_id,
        userId,
        limit,
        offset,
      );

      res.json(result);
    } catch (error) {
      logger.error("Get Aptos local tx history error:", error);
      const errorMessage =
        error instanceof Error
          ? error.message
          : "Failed to get transaction history";
      if (errorMessage.includes("Unauthorized")) {
        return res.status(403).json({ error: errorMessage });
      }
      if (errorMessage.includes("not found")) {
        return res.status(404).json({ error: errorMessage });
      }
      res.status(500).json({ error: errorMessage });
    }
  },
);

aptosTransactionRoutes.get(
  "/account/:address",
  authenticate,
  generalLimiter,
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
          `aptos/transactions/account/${address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await aptosTransactionManager.getAccountTransactions(
        address,
        network as string,
      );

      res.json({
        account_address: address,
        total: result.length,
        transactions: result,
      });
    } catch (error) {
      logger.error("Get Aptos account transactions error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get transactions",
      });
    }
  },
);

aptosTransactionRoutes.get("/:hash", generalLimiter, async (req: any, res) => {
  try {
    const { hash } = req.params;
    const { network } = req.query;

    const result = await aptosTransactionManager.getTransaction(
      hash,
      network as string,
    );
    res.json(result);
  } catch (error) {
    logger.error("Get Aptos transaction error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Failed to get transaction";
    if (errorMessage.includes("not found") || errorMessage.includes("404")) {
      return res.status(404).json({ error: "Transaction not found" });
    }
    res.status(500).json({ error: errorMessage });
  }
});
