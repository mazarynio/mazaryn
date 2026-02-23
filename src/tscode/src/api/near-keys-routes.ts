import express from "express";
import { nearKeyManager } from "../services/near/keys.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import {
  transferLimiter,
  generalLimiter,
  exportKeyLimiter,
} from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  AddFullAccessKeyRequest,
  AddFunctionCallKeyRequest,
  DeleteKeyRequest,
  ImportFromSeedPhraseRequest,
} from "../core/near-types.js";

export const nearKeyRoutes = express.Router();

nearKeyRoutes.get(
  "/wallet/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const result = await nearKeyManager.getAccessKeys(wallet_id, userId);
      res.json(result);
    } catch (error) {
      logger.error("Get access keys error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to get access keys";

      if (errorMessage === "Unauthorized") {
        return res.status(403).json({ error: "Unauthorized" });
      }
      if (errorMessage === "Wallet not found") {
        return res.status(404).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearKeyRoutes.get(
  "/account/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result = await nearKeyManager.getAccessKeysByAccountId(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Get access keys by account error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get access keys",
      });
    }
  },
);

nearKeyRoutes.post(
  "/wallet/:wallet_id/stored",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const keys = await nearKeyManager.getStoredKeys(wallet_id, userId);

      res.json({
        wallet_id,
        total: keys.length,
        keys: keys.map((k) => ({
          key_id: k.key_id,
          public_key: k.public_key,
          key_type: k.key_type,
          contract_id: k.contract_id,
          method_names: k.method_names,
          allowance_near: k.allowance_near,
          label: k.label,
          created_at: k.created_at,
        })),
      });
    } catch (error) {
      logger.error("Get stored keys error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to get stored keys";

      if (errorMessage === "Unauthorized") {
        return res.status(403).json({ error: "Unauthorized" });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearKeyRoutes.post(
  "/add/full-access",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, label } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/keys/add/full-access",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AddFullAccessKeyRequest = { wallet_id, label };
      const result = await nearKeyManager.addFullAccessKey(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_add_full_access_key",
          account_id: result.account_id,
          new_public_key: result.new_public_key,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Add full access key error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to add full access key",
      });
    }
  },
);

nearKeyRoutes.post(
  "/add/function-call",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, contract_id, method_names, allowance_near, label } =
        req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }

      if (!contract_id) {
        return res.status(400).json({ error: "contract_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/keys/add/function-call",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AddFunctionCallKeyRequest = {
        wallet_id,
        contract_id,
        method_names,
        allowance_near,
        label,
      };

      const result = await nearKeyManager.addFunctionCallKey(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_add_function_call_key",
          account_id: result.account_id,
          new_public_key: result.new_public_key,
          contract_id: result.contract_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Add function call key error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to add function call key",
      });
    }
  },
);

nearKeyRoutes.delete(
  "/wallet/:wallet_id/key",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { public_key } = req.body;

      if (!public_key) {
        return res.status(400).json({ error: "public_key is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/keys/wallet/${wallet_id}/key`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: DeleteKeyRequest = { wallet_id, public_key };
      const result = await nearKeyManager.deleteKey(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_delete_key",
          account_id: result.account_id,
          deleted_public_key: result.deleted_public_key,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Delete key error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to delete key",
      });
    }
  },
);

nearKeyRoutes.get(
  "/seed-phrase/generate",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const result = nearKeyManager.generateSeedPhrase();

      auditLogger.logSuspiciousActivity(
        req.user.user_id,
        req.ip || "unknown",
        "NEAR seed phrase generated",
        {},
      );

      res.json({
        ...result,
        warning:
          "Store this seed phrase securely. It grants full access to the account. Never share it.",
      });
    } catch (error) {
      logger.error("Generate seed phrase error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to generate seed phrase",
      });
    }
  },
);

nearKeyRoutes.post(
  "/seed-phrase/import",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { account_id, seed_phrase, account_name } = req.body;

      if (!account_id) {
        return res.status(400).json({ error: "account_id is required" });
      }

      if (!seed_phrase) {
        return res.status(400).json({ error: "seed_phrase is required" });
      }

      const words = seed_phrase.trim().split(/\s+/);
      if (words.length !== 12 && words.length !== 24) {
        return res.status(400).json({
          error: "seed_phrase must be 12 or 24 words",
        });
      }

      const request: ImportFromSeedPhraseRequest = {
        user_id: userId,
        account_id,
        seed_phrase: seed_phrase.trim(),
        account_name,
      };

      const result = await nearKeyManager.importFromSeedPhrase(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "WALLET_CREATED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_import_from_seed_phrase",
          account_id: result.account_id,
          wallet_id: result.wallet_id,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Import from seed phrase error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to import account";

      if (errorMessage.includes("already imported")) {
        return res.status(409).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);
