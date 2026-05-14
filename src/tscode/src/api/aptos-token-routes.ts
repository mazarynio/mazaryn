import express from "express";
import { aptosTokenManager } from "../services/aptos/token.js";
import { aptosDatabase } from "../services/database/aptos-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  AptosGetCoinBalanceRequest,
  AptosGetFAMetadataRequest,
  AptosGetFABalancesRequest,
  AptosTransferCoinRequest,
  AptosTransferFungibleAssetRequest,
  AptosMintCoinRequest,
  AptosViewFunctionRequest,
  AptosGetAccountCoinsRequest,
} from "../core/aptos-types.js";

export const aptosTokenRoutes = express.Router();

aptosTokenRoutes.get(
  "/coin-balance/:address",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { address } = req.params;
      const { coin_type, network } = req.query;

      if (!coin_type) {
        return res
          .status(400)
          .json({ error: "coin_type query parameter is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `aptos/tokens/coin-balance/${address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosGetCoinBalanceRequest = {
        account_address: address,
        coin_type: coin_type as string,
        network: network as string | undefined,
      };

      const result = await aptosTokenManager.getCoinBalance(request);
      res.json(result);
    } catch (error) {
      logger.error("Get Aptos coin balance error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get coin balance",
      });
    }
  },
);

aptosTokenRoutes.get("/fa-metadata", generalLimiter, async (req: any, res) => {
  try {
    const { asset_type, network } = req.query;

    if (!asset_type) {
      return res
        .status(400)
        .json({ error: "asset_type query parameter is required" });
    }

    const request: AptosGetFAMetadataRequest = {
      asset_type: asset_type as string,
      network: network as string | undefined,
    };

    const result = await aptosTokenManager.getFAMetadata(request);
    res.json(result);
  } catch (error) {
    logger.error("Get Aptos FA metadata error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Failed to get FA metadata";
    if (errorMessage.includes("not found")) {
      return res.status(404).json({ error: errorMessage });
    }
    res.status(500).json({ error: errorMessage });
  }
});

aptosTokenRoutes.get(
  "/fa-balances/:address",
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
          `aptos/tokens/fa-balances/${address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosGetFABalancesRequest = {
        account_address: address,
        network: network as string | undefined,
      };

      const result = await aptosTokenManager.getFABalances(request);
      res.json(result);
    } catch (error) {
      logger.error("Get Aptos FA balances error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get FA balances",
      });
    }
  },
);

aptosTokenRoutes.get(
  "/coins/:address",
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
          `aptos/tokens/coins/${address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosGetAccountCoinsRequest = {
        account_address: address,
        network: network as string | undefined,
      };

      const result = await aptosTokenManager.getAccountCoins(request);
      res.json(result);
    } catch (error) {
      logger.error("Get Aptos account coins error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get account coins",
      });
    }
  },
);

aptosTokenRoutes.post(
  "/transfer-coin",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        sender_address,
        recipient_address,
        amount,
        coin_type,
        network,
        options,
      } = req.body;

      if (!sender_address) {
        return res.status(400).json({ error: "sender_address is required" });
      }
      if (!recipient_address) {
        return res.status(400).json({ error: "recipient_address is required" });
      }
      if (amount === undefined || amount <= 0) {
        return res
          .status(400)
          .json({ error: "amount must be a positive number" });
      }
      if (!coin_type) {
        return res.status(400).json({ error: "coin_type is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(sender_address);
      if (!wallet) {
        return res.status(404).json({ error: "Sender wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/tokens/transfer-coin",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosTransferCoinRequest = {
        sender_address,
        recipient_address,
        amount,
        coin_type,
        network,
        options,
      };

      const result = await aptosTokenManager.transferCoin(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_coin_transfer",
          sender_address,
          recipient_address,
          amount,
          coin_type,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos coin transfer error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to transfer coin",
      });
    }
  },
);

aptosTokenRoutes.post(
  "/transfer-fa",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        sender_address,
        recipient_address,
        metadata_address,
        amount,
        network,
      } = req.body;

      if (!sender_address) {
        return res.status(400).json({ error: "sender_address is required" });
      }
      if (!recipient_address) {
        return res.status(400).json({ error: "recipient_address is required" });
      }
      if (!metadata_address) {
        return res.status(400).json({ error: "metadata_address is required" });
      }
      if (amount === undefined || amount <= 0) {
        return res
          .status(400)
          .json({ error: "amount must be a positive number" });
      }

      const wallet = aptosDatabase.getWalletByAddress(sender_address);
      if (!wallet) {
        return res.status(404).json({ error: "Sender wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/tokens/transfer-fa",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosTransferFungibleAssetRequest = {
        sender_address,
        recipient_address,
        metadata_address,
        amount,
        network,
      };

      const result = await aptosTokenManager.transferFungibleAsset(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_fa_transfer",
          sender_address,
          recipient_address,
          metadata_address,
          amount,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos FA transfer error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to transfer fungible asset",
      });
    }
  },
);

aptosTokenRoutes.post(
  "/mint-coin",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        admin_address,
        recipient_address,
        amount,
        coin_module_address,
        coin_module_name,
        network,
      } = req.body;

      if (!admin_address) {
        return res.status(400).json({ error: "admin_address is required" });
      }
      if (!recipient_address) {
        return res.status(400).json({ error: "recipient_address is required" });
      }
      if (amount === undefined || amount <= 0) {
        return res
          .status(400)
          .json({ error: "amount must be a positive number" });
      }
      if (!coin_module_address) {
        return res
          .status(400)
          .json({ error: "coin_module_address is required" });
      }
      if (!coin_module_name) {
        return res.status(400).json({ error: "coin_module_name is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(admin_address);
      if (!wallet) {
        return res.status(404).json({ error: "Admin wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/tokens/mint-coin",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosMintCoinRequest = {
        admin_address,
        recipient_address,
        amount,
        coin_module_address,
        coin_module_name,
        network,
      };

      const result = await aptosTokenManager.mintCoin(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_coin_mint",
          admin_address,
          recipient_address,
          amount,
          coin_module_address,
          coin_module_name,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos coin mint error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to mint coin",
      });
    }
  },
);

aptosTokenRoutes.post("/view", generalLimiter, async (req: any, res) => {
  try {
    const {
      function: fn,
      type_arguments,
      function_arguments,
      network,
    } = req.body;

    if (!fn) {
      return res.status(400).json({ error: "function is required" });
    }

    const request: AptosViewFunctionRequest = {
      function: fn,
      type_arguments,
      function_arguments,
      network,
    };

    const result = await aptosTokenManager.viewFunction(request);
    res.json(result);
  } catch (error) {
    logger.error("Aptos view function error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to call view function",
    });
  }
});

aptosTokenRoutes.get(
  "/fa-activities",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { asset_type, limit, network } = req.query;

      if (!asset_type) {
        return res
          .status(400)
          .json({ error: "asset_type query parameter is required" });
      }

      const parsedLimit = limit ? parseInt(limit as string, 10) : 10;
      if (isNaN(parsedLimit) || parsedLimit <= 0 || parsedLimit > 100) {
        return res
          .status(400)
          .json({ error: "limit must be a positive integer up to 100" });
      }

      const result = await aptosTokenManager.getFungibleAssetActivities(
        asset_type as string,
        parsedLimit,
        network as string | undefined,
      );

      res.json({
        asset_type,
        total: result.length,
        activities: result,
      });
    } catch (error) {
      logger.error("Get Aptos FA activities error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get FA activities",
      });
    }
  },
);

aptosTokenRoutes.get(
  "/fa-metadata/creator/:creator_address",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { creator_address } = req.params;
      const { network } = req.query;

      const result = await aptosTokenManager.getAllFungibleAssetMetadata(
        creator_address,
        network as string | undefined,
      );

      res.json({
        creator_address,
        total: result.length,
        assets: result,
      });
    } catch (error) {
      logger.error("Get Aptos FA metadata by creator error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get FA metadata",
      });
    }
  },
);
