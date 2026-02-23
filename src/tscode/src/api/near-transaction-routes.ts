import express from "express";
import { nearTransactionManager } from "../services/near/transactions.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  SignAndSendTransactionRequest,
  CallFunctionRequest,
  ViewFunctionRequest,
  CreateAndSignTransactionRequest,
} from "../core/near-types.js";

export const nearTransactionRoutes = express.Router();

nearTransactionRoutes.post(
  "/send",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { from_wallet_id, receiver_id, actions, wait_until } = req.body;

      if (!from_wallet_id)
        return res.status(400).json({ error: "from_wallet_id is required" });
      if (!receiver_id)
        return res.status(400).json({ error: "receiver_id is required" });
      if (!Array.isArray(actions) || actions.length === 0)
        return res
          .status(400)
          .json({ error: "actions array is required and must not be empty" });

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/tx/send",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: SignAndSendTransactionRequest = {
        from_wallet_id,
        receiver_id,
        actions,
        wait_until,
      };

      const result = await nearTransactionManager.signAndSendTransaction(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_batch_transaction",
          from_account_id: result.from_account_id,
          receiver_id: result.receiver_id,
          actions_count: result.actions_count,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Batch transaction error:", error);
      const msg = error instanceof Error ? error.message : "Transaction failed";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearTransactionRoutes.post(
  "/call",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        from_wallet_id,
        contract_id,
        method_name,
        args,
        gas_tera,
        deposit_near,
        wait_until,
      } = req.body;

      if (!from_wallet_id)
        return res.status(400).json({ error: "from_wallet_id is required" });
      if (!contract_id)
        return res.status(400).json({ error: "contract_id is required" });
      if (!method_name)
        return res.status(400).json({ error: "method_name is required" });

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/tx/call",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: CallFunctionRequest = {
        from_wallet_id,
        contract_id,
        method_name,
        args,
        gas_tera,
        deposit_near,
        wait_until,
      };

      const result = await nearTransactionManager.callFunction(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_contract_call",
          from_account_id: result.from_account_id,
          contract_id: result.contract_id,
          method_name: result.method_name,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Contract call error:", error);
      const msg =
        error instanceof Error ? error.message : "Contract call failed";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearTransactionRoutes.post("/view", generalLimiter, async (req: any, res) => {
  try {
    const { contract_id, method_name, args } = req.body;

    if (!contract_id)
      return res.status(400).json({ error: "contract_id is required" });
    if (!method_name)
      return res.status(400).json({ error: "method_name is required" });

    const request: ViewFunctionRequest = { contract_id, method_name, args };
    const result = await nearTransactionManager.viewFunction(request);

    res.json(result);
  } catch (error) {
    logger.error("View function error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "View call failed",
    });
  }
});

nearTransactionRoutes.post(
  "/sign",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { from_wallet_id, receiver_id, actions } = req.body;

      if (!from_wallet_id)
        return res.status(400).json({ error: "from_wallet_id is required" });
      if (!receiver_id)
        return res.status(400).json({ error: "receiver_id is required" });
      if (!Array.isArray(actions) || actions.length === 0)
        return res
          .status(400)
          .json({ error: "actions array is required and must not be empty" });

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/tx/sign",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: CreateAndSignTransactionRequest = {
        from_wallet_id,
        receiver_id,
        actions,
      };

      const result = await nearTransactionManager.createAndSignTransaction(
        request,
        userId,
      );

      auditLogger.logSuspiciousActivity(
        userId,
        req.ip || "unknown",
        "NEAR transaction signed but not broadcast",
        {
          from_account_id: result.from_account_id,
          receiver_id: result.receiver_id,
        },
      );

      res.json(result);
    } catch (error) {
      logger.error("Sign transaction error:", error);
      const msg = error instanceof Error ? error.message : "Signing failed";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearTransactionRoutes.post(
  "/broadcast",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const { signed_transaction } = req.body;

      if (!signed_transaction) {
        return res
          .status(400)
          .json({ error: "signed_transaction is required" });
      }

      const result =
        await nearTransactionManager.sendSignedTransaction(signed_transaction);

      res.json({
        transaction_hash: result?.transaction?.hash ?? "unknown",
        status: "broadcast",
        timestamp: Date.now(),
        raw: result,
      });
    } catch (error) {
      logger.error("Broadcast transaction error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Broadcast failed",
      });
    }
  },
);

nearTransactionRoutes.get(
  "/status/:tx_hash",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { tx_hash } = req.params;
      const { account_id } = req.query;

      if (!account_id) {
        return res
          .status(400)
          .json({ error: "account_id query param is required" });
      }

      const result = await nearTransactionManager.getTransactionStatus(
        tx_hash,
        account_id as string,
      );

      res.json(result);
    } catch (error) {
      logger.error("Get transaction status error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get tx status",
      });
    }
  },
);

nearTransactionRoutes.get(
  "/history/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const limit = Math.min(
        parseInt((req.query.limit as string) || "20", 10),
        100,
      );
      const offset = parseInt((req.query.offset as string) || "0", 10);

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) return res.status(404).json({ error: "Wallet not found" });
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/tx/history/${wallet_id}`,
        );
        return res.status(403).json({ error: "Unauthorized" });
      }

      const result = await nearTransactionManager.getUserTransactions(
        wallet_id,
        userId,
        limit,
        offset,
      );

      res.json(result);
    } catch (error) {
      logger.error("Transaction history error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get history",
      });
    }
  },
);
