import express from "express";
import { airdropManager } from "../services/solana/airdrop.js";
import { walletManager } from "../services/solana/wallet.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type { CreateAirdropRequest } from "../core/types.js";

export const airdropRoutes = express.Router();

airdropRoutes.post(
  "/create",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const request = req.body as CreateAirdropRequest;

      if (!request.wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }

      if (!request.type) {
        return res
          .status(400)
          .json({ error: "type is required (sol, token, or nft)" });
      }

      if (!["sol", "token", "nft"].includes(request.type)) {
        return res
          .status(400)
          .json({ error: "type must be sol, token, or nft" });
      }

      if (!request.recipients || request.recipients.length === 0) {
        return res
          .status(400)
          .json({ error: "recipients array is required and cannot be empty" });
      }

      if (request.recipients.length > 1000) {
        return res
          .status(400)
          .json({ error: "Maximum 1000 recipients per airdrop" });
      }

      if (request.type === "sol" && !request.amount_per_recipient) {
        return res
          .status(400)
          .json({ error: "amount_per_recipient is required for SOL airdrops" });
      }

      if (request.type === "token" && !request.token_mint) {
        return res
          .status(400)
          .json({ error: "token_mint is required for token airdrops" });
      }

      if (request.type === "token" && !request.amount_per_recipient) {
        return res
          .status(400)
          .json({
            error: "amount_per_recipient is required for token airdrops",
          });
      }

      if (request.type === "nft") {
        const missingMint = request.recipients.find((r) => !r.mint_address);
        if (missingMint) {
          return res
            .status(400)
            .json({
              error:
                "mint_address is required for each recipient in NFT airdrops",
            });
        }
      }

      const walletInfo = await walletManager.getWalletInfo(request.wallet_id);
      if (!walletInfo) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (walletInfo.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "airdrop creation",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const signerInfo = walletManager.getWalletSigner(request.wallet_id);
      if (!signerInfo) {
        return res.status(404).json({ error: "Wallet signer not found" });
      }

      logger.info(
        `Airdrop request: type=${request.type}, recipients=${request.recipients.length}, wallet=${request.wallet_id}`,
      );

      const result = await airdropManager.createAirdrop(
        signerInfo.signer,
        signerInfo.publicKey,
        userId,
        request,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          airdrop_id: result.airdrop_id,
          type: result.type,
          total_recipients: result.total_recipients,
          successful: result.successful,
          failed: result.failed,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Airdrop creation error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Airdrop failed";

      if (errorMessage.includes("Insufficient SOL balance")) {
        return res.status(400).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

airdropRoutes.get("/:airdrop_id", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const airdropId = req.params.airdrop_id;

    const airdrop = await airdropManager.getAirdrop(airdropId);

    if (!airdrop) {
      return res.status(404).json({ error: "Airdrop not found" });
    }

    if (airdrop.user_id !== userId) {
      auditLogger.logUnauthorizedAccess(
        userId,
        req.ip || "unknown",
        `airdrop/${airdropId}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: airdrop does not belong to user" });
    }

    res.json(airdrop);
  } catch (error) {
    logger.error("Get airdrop error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Failed to get airdrop";

    if (errorMessage === "Airdrop not found") {
      return res.status(404).json({ error: errorMessage });
    }

    res.status(500).json({ error: errorMessage });
  }
});

airdropRoutes.get("/user/:user_id", authenticate, async (req: any, res) => {
  try {
    const requestingUserId = req.user.user_id;
    const targetUserId = req.params.user_id;

    if (requestingUserId !== targetUserId) {
      auditLogger.logUnauthorizedAccess(
        requestingUserId,
        req.ip || "unknown",
        `airdrops/user/${targetUserId}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: cannot access other user airdrops" });
    }

    const airdrops = await airdropManager.getUserAirdrops(targetUserId);

    res.json({
      user_id: targetUserId,
      total: airdrops.length,
      airdrops,
    });
  } catch (error) {
    logger.error("Get user airdrops error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get user airdrops",
    });
  }
});

airdropRoutes.get("/wallet/:wallet_id", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const walletId = req.params.wallet_id;

    const walletInfo = await walletManager.getWalletInfo(walletId);
    if (!walletInfo) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (walletInfo.user_id !== userId) {
      auditLogger.logUnauthorizedAccess(
        userId,
        req.ip || "unknown",
        `airdrops/wallet/${walletId}`,
      );
      return res
        .status(403)
        .json({ error: "Unauthorized: wallet does not belong to user" });
    }

    const airdrops = await airdropManager.getWalletAirdrops(walletId);

    res.json({
      wallet_id: walletId,
      total: airdrops.length,
      airdrops,
    });
  } catch (error) {
    logger.error("Get wallet airdrops error:", error);
    res.status(500).json({
      error:
        error instanceof Error
          ? error.message
          : "Failed to get wallet airdrops",
    });
  }
});

airdropRoutes.post("/validate", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const request = req.body as CreateAirdropRequest;

    if (!request.wallet_id) {
      return res.status(400).json({ error: "wallet_id is required" });
    }

    if (!request.type || !["sol", "token", "nft"].includes(request.type)) {
      return res
        .status(400)
        .json({ error: "Valid type is required (sol, token, or nft)" });
    }

    if (!request.recipients || request.recipients.length === 0) {
      return res.status(400).json({ error: "Recipients are required" });
    }

    const walletInfo = await walletManager.getWalletInfo(request.wallet_id);
    if (!walletInfo) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (walletInfo.user_id !== userId) {
      return res
        .status(403)
        .json({ error: "Unauthorized: wallet does not belong to user" });
    }

    const balance = await walletManager.getBalance(walletInfo.public_key);

    let estimatedCost = 0;
    const warnings = [];

    if (request.type === "sol") {
      const amountPerRecipient = request.amount_per_recipient || 0;
      const totalAmount = amountPerRecipient * request.recipients.length;
      const estimatedFees = request.recipients.length * 5000;
      estimatedCost = totalAmount + estimatedFees;

      if (balance.balance_lamports < estimatedCost) {
        warnings.push(
          `Insufficient balance. Need ${estimatedCost / 1_000_000_000} SOL, have ${balance.balance_sol} SOL`,
        );
      }
    } else if (request.type === "token") {
      const estimatedFees = request.recipients.length * 10000;
      estimatedCost = estimatedFees;

      if (balance.balance_lamports < estimatedCost) {
        warnings.push(
          `Insufficient SOL for fees. Need ${estimatedCost / 1_000_000_000} SOL, have ${balance.balance_sol} SOL`,
        );
      }
    } else if (request.type === "nft") {
      const estimatedFees = request.recipients.length * 10000;
      estimatedCost = estimatedFees;

      if (balance.balance_lamports < estimatedCost) {
        warnings.push(
          `Insufficient SOL for fees. Need ${estimatedCost / 1_000_000_000} SOL, have ${balance.balance_sol} SOL`,
        );
      }
    }

    res.json({
      valid: warnings.length === 0,
      wallet_id: request.wallet_id,
      type: request.type,
      total_recipients: request.recipients.length,
      estimated_cost_lamports: estimatedCost,
      estimated_cost_sol: estimatedCost / 1_000_000_000,
      current_balance_lamports: balance.balance_lamports,
      current_balance_sol: balance.balance_sol,
      warnings,
    });
  } catch (error) {
    logger.error("Airdrop validation error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Validation failed",
    });
  }
});
