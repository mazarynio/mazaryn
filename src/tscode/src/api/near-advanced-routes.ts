import express from "express";
import { nearAdvancedManager } from "../services/near/advanced.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  CreateMetaTransactionRequest,
  RelayMetaTransactionRequest,
  FundImplicitAccountRequest,
  AddMultipleKeysRequest,
  MultiKeyTransferRequest,
} from "../core/near-types.js";

export const nearAdvancedRoutes = express.Router();

nearAdvancedRoutes.post(
  "/meta/create",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { from_wallet_id, receiver_id, actions, block_height_ttl } =
        req.body;

      if (!from_wallet_id) {
        return res.status(400).json({ error: "from_wallet_id is required" });
      }
      if (!receiver_id) {
        return res.status(400).json({ error: "receiver_id is required" });
      }
      if (!Array.isArray(actions) || actions.length === 0) {
        return res
          .status(400)
          .json({ error: "actions array is required and must not be empty" });
      }

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/advanced/meta/create",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: CreateMetaTransactionRequest = {
        from_wallet_id,
        receiver_id,
        actions,
        block_height_ttl,
      };

      const result = await nearAdvancedManager.createMetaTransaction(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_meta_transaction_created",
          from_account_id: result.from_account_id,
          receiver_id: result.receiver_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Create meta-transaction error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to create meta-transaction";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearAdvancedRoutes.post(
  "/meta/relay",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { relayer_wallet_id, signed_delegate } = req.body;

      if (!relayer_wallet_id) {
        return res.status(400).json({ error: "relayer_wallet_id is required" });
      }
      if (!signed_delegate) {
        return res.status(400).json({ error: "signed_delegate is required" });
      }

      const wallet = nearDatabase.getWallet(relayer_wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Relayer wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/advanced/meta/relay",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: RelayMetaTransactionRequest = {
        relayer_wallet_id,
        signed_delegate,
      };

      const result = await nearAdvancedManager.relayMetaTransaction(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_meta_transaction_relayed",
          relayer_account_id: result.relayer_account_id,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Relay meta-transaction error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to relay meta-transaction";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Relayer wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearAdvancedRoutes.post(
  "/implicit/create",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;

      const result = await nearAdvancedManager.createImplicitAccount(userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_implicit_account_created",
          account_id: result.account_id,
          wallet_id: result.wallet_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Create implicit account error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to create implicit account",
      });
    }
  },
);

nearAdvancedRoutes.post(
  "/implicit/fund",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { from_wallet_id, implicit_account_id, amount_yocto } = req.body;

      if (!from_wallet_id) {
        return res.status(400).json({ error: "from_wallet_id is required" });
      }
      if (!implicit_account_id) {
        return res
          .status(400)
          .json({ error: "implicit_account_id is required" });
      }

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Funding wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/advanced/implicit/fund",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: FundImplicitAccountRequest = {
        from_wallet_id,
        implicit_account_id,
        amount_yocto,
      };

      const result = await nearAdvancedManager.fundImplicitAccount(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_implicit_account_funded",
          from_account_id: result.from_account_id,
          implicit_account_id: result.implicit_account_id,
          amount_yocto: result.amount_yocto,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Fund implicit account error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to fund implicit account";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Funding wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearAdvancedRoutes.post(
  "/multikey/add",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, key_count, label_prefix } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!key_count || typeof key_count !== "number") {
        return res.status(400).json({ error: "key_count must be a number" });
      }
      if (key_count < 1 || key_count > 50) {
        return res
          .status(400)
          .json({ error: "key_count must be between 1 and 50" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/advanced/multikey/add",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AddMultipleKeysRequest = {
        wallet_id,
        key_count,
        label_prefix,
      };

      const result = await nearAdvancedManager.addMultipleKeys(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_multiple_keys_added",
          account_id: result.account_id,
          keys_added: result.keys_added,
          transaction_hash: result.transaction_hash,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Add multiple keys error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to add multiple keys";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearAdvancedRoutes.post(
  "/multikey/transfer",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, transfers } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!Array.isArray(transfers) || transfers.length === 0) {
        return res
          .status(400)
          .json({ error: "transfers array is required and must not be empty" });
      }
      if (transfers.length > 100) {
        return res
          .status(400)
          .json({ error: "transfers array cannot exceed 100 items" });
      }

      for (const t of transfers) {
        if (!t.receiver_id) {
          return res
            .status(400)
            .json({ error: "Each transfer must have a receiver_id" });
        }
        if (!t.amount_near) {
          return res
            .status(400)
            .json({ error: "Each transfer must have an amount_near" });
        }
        if (
          isNaN(parseFloat(t.amount_near)) ||
          parseFloat(t.amount_near) <= 0
        ) {
          return res
            .status(400)
            .json({
              error: `Invalid amount_near for receiver ${t.receiver_id}`,
            });
        }
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/advanced/multikey/transfer",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: MultiKeyTransferRequest = {
        wallet_id,
        transfers,
      };

      const result = await nearAdvancedManager.multiKeyTransfer(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_multi_key_transfer",
          account_id: result.account_id,
          total_transfers: result.total_transfers,
          successful: result.successful,
          failed: result.failed,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Multi-key transfer error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to execute multi-key transfers";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      if (msg.includes("No stored access keys"))
        return res.status(400).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);
