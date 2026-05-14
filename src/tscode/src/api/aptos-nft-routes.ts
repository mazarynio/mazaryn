import express from "express";
import { aptosNFTManager } from "../services/aptos/nft.js";
import { aptosDatabase } from "../services/database/aptos-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  AptosCreateCollectionRequest,
  AptosMintNFTRequest,
  AptosTransferNFTRequest,
  AptosBurnNFTRequest,
} from "../core/aptos-types.js";

export const aptosNFTRoutes = express.Router();

aptosNFTRoutes.post(
  "/collections/create",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        account_address,
        name,
        description,
        uri,
        max_supply,
        mutable_description,
        mutable_uri,
        mutable_token_description,
        mutable_token_name,
        mutable_token_properties,
        mutable_token_uri,
        tokens_burnable_by_creator,
        tokens_freezable_by_creator,
        royalty_numerator,
        royalty_denominator,
        network,
      } = req.body;

      if (!account_address) {
        return res.status(400).json({ error: "account_address is required" });
      }

      if (!name) {
        return res.status(400).json({ error: "name is required" });
      }

      if (!description) {
        return res.status(400).json({ error: "description is required" });
      }

      if (!uri) {
        return res.status(400).json({ error: "uri is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(account_address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/nft/collections/create",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosCreateCollectionRequest = {
        account_address,
        name,
        description,
        uri,
        max_supply,
        mutable_description,
        mutable_uri,
        mutable_token_description,
        mutable_token_name,
        mutable_token_properties,
        mutable_token_uri,
        tokens_burnable_by_creator,
        tokens_freezable_by_creator,
        royalty_numerator,
        royalty_denominator,
      };

      const result = await aptosNFTManager.createCollection(request, network);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_collection_created",
          collection_name: name,
          creator_address: account_address,
          transaction_hash: result.transaction_hash,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Aptos create collection error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to create collection",
      });
    }
  },
);

aptosNFTRoutes.post(
  "/tokens/mint",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        account_address,
        collection,
        name,
        description,
        uri,
        property_keys,
        property_types,
        property_values,
        network,
      } = req.body;

      if (!account_address) {
        return res.status(400).json({ error: "account_address is required" });
      }

      if (!collection) {
        return res.status(400).json({ error: "collection is required" });
      }

      if (!name) {
        return res.status(400).json({ error: "name is required" });
      }

      if (!description) {
        return res.status(400).json({ error: "description is required" });
      }

      if (!uri) {
        return res.status(400).json({ error: "uri is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(account_address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/nft/tokens/mint",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosMintNFTRequest = {
        account_address,
        collection,
        name,
        description,
        uri,
        property_keys,
        property_types,
        property_values,
      };

      const result = await aptosNFTManager.mintToken(request, network);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_token_minted",
          collection,
          token_name: name,
          creator_address: account_address,
          transaction_hash: result.transaction_hash,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Aptos mint token error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to mint token",
      });
    }
  },
);

aptosNFTRoutes.post(
  "/tokens/transfer",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { account_address, token_address, recipient_address, network } =
        req.body;

      if (!account_address) {
        return res.status(400).json({ error: "account_address is required" });
      }

      if (!token_address) {
        return res.status(400).json({ error: "token_address is required" });
      }

      if (!recipient_address) {
        return res.status(400).json({ error: "recipient_address is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(account_address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/nft/tokens/transfer",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosTransferNFTRequest = {
        account_address,
        token_address,
        recipient_address,
        network,
      };

      const result = await aptosNFTManager.transferToken(request, network);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_token_transferred",
          token_address,
          from: account_address,
          to: recipient_address,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos transfer token error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to transfer token",
      });
    }
  },
);

aptosNFTRoutes.post(
  "/tokens/burn",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { account_address, token_address, network } = req.body;

      if (!account_address) {
        return res.status(400).json({ error: "account_address is required" });
      }

      if (!token_address) {
        return res.status(400).json({ error: "token_address is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(account_address);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/nft/tokens/burn",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosBurnNFTRequest = {
        account_address,
        token_address,
        network,
      };
      const result = await aptosNFTManager.burnToken(request, network);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_token_burned",
          token_address,
          owner_address: account_address,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos burn token error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to burn token",
      });
    }
  },
);

aptosNFTRoutes.get(
  "/tokens/owned/:address",
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
          `aptos/nft/tokens/owned/${address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const tokens = await aptosNFTManager.getOwnedTokens(
        address,
        network as string,
      );
      res.json({ account_address: address, total: tokens.length, tokens });
    } catch (error) {
      logger.error("Get Aptos owned tokens error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get owned tokens",
      });
    }
  },
);
