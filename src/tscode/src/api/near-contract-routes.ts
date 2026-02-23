import express from "express";
import { nearContractManager } from "../services/near/contracts.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  DeployContractRequest,
  DeployGlobalContractRequest,
  UseGlobalContractRequest,
} from "../core/near-types.js";

export const nearContractRoutes = express.Router();

nearContractRoutes.post(
  "/deploy",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, wasm_base64 } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!wasm_base64) {
        return res.status(400).json({ error: "wasm_base64 is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/contracts/deploy",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: DeployContractRequest = { wallet_id, wasm_base64 };
      const result = await nearContractManager.deployContract(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_contract_deployed",
          account_id: result.account_id,
          transaction_hash: result.transaction_hash,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Deploy contract error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to deploy contract";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearContractRoutes.post(
  "/deploy-global",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, wasm_base64, deploy_mode } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!wasm_base64) {
        return res.status(400).json({ error: "wasm_base64 is required" });
      }
      if (!deploy_mode) {
        return res
          .status(400)
          .json({ error: "deploy_mode is required (accountId or codeHash)" });
      }
      if (!["accountId", "codeHash"].includes(deploy_mode)) {
        return res
          .status(400)
          .json({ error: "deploy_mode must be accountId or codeHash" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/contracts/deploy-global",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: DeployGlobalContractRequest = {
        wallet_id,
        wasm_base64,
        deploy_mode,
      };
      const result = await nearContractManager.deployGlobalContract(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_global_contract_deployed",
          account_id: result.account_id,
          deploy_mode: result.deploy_mode,
          transaction_hash: result.transaction_hash,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Deploy global contract error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to deploy global contract";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearContractRoutes.post(
  "/use-global",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, identifier_type, account_id, code_hash } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!identifier_type) {
        return res
          .status(400)
          .json({
            error: "identifier_type is required (accountId or codeHash)",
          });
      }
      if (!["accountId", "codeHash"].includes(identifier_type)) {
        return res
          .status(400)
          .json({ error: "identifier_type must be accountId or codeHash" });
      }
      if (identifier_type === "accountId" && !account_id) {
        return res
          .status(400)
          .json({
            error: "account_id is required when identifier_type is accountId",
          });
      }
      if (identifier_type === "codeHash" && !code_hash) {
        return res
          .status(400)
          .json({
            error: "code_hash is required when identifier_type is codeHash",
          });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/contracts/use-global",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: UseGlobalContractRequest = {
        wallet_id,
        identifier_type,
        account_id,
        code_hash,
      };

      const result = await nearContractManager.useGlobalContract(
        request,
        userId,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_global_contract_used",
          account_id: result.account_id,
          identifier_type: result.identifier_type,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Use global contract error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to use global contract";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearContractRoutes.get(
  "/code/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;

      const result = await nearContractManager.getContractCode(
        wallet_id,
        userId,
      );
      res.json(result);
    } catch (error) {
      logger.error("Get contract code error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to get contract code";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearContractRoutes.get(
  "/code/account/:account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const result =
        await nearContractManager.getContractCodeByAccountId(account_id);
      res.json(result);
    } catch (error) {
      logger.error("Get contract code by account error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get contract code",
      });
    }
  },
);

nearContractRoutes.get(
  "/state/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { prefix } = req.query;

      const result = await nearContractManager.getContractState(
        wallet_id,
        userId,
        prefix as string | undefined,
      );
      res.json(result);
    } catch (error) {
      logger.error("Get contract state error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to get contract state";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);
