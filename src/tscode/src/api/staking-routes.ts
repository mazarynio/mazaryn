import express from "express";
import { stakingManager } from "../services/solana/staking.js";
import { walletDatabase } from "../services/database/database.js";
import { encryptionService } from "../services/security/encryption.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import { createKeyPairSignerFromBytes } from "@solana/kit";
import type {
  CreateStakeAccountRequest,
  DeactivateStakeRequest,
  WithdrawStakeRequest,
} from "../core/types.js";

export const stakingRoutes = express.Router();

async function getSignerForWallet(
  walletId: string,
  userId: string,
): Promise<any> {
  const wallet = walletDatabase.getWallet(walletId);
  if (!wallet) {
    throw new Error("Wallet not found");
  }
  if (wallet.user_id !== userId) {
    throw new Error("Access denied");
  }

  const privateKeyBytes = encryptionService.decrypt(
    wallet.encrypted_private_key,
    wallet.encryption_iv,
    wallet.encryption_tag,
  );

  const keyBytes = Buffer.from(privateKeyBytes, "hex");
  const signer = await createKeyPairSignerFromBytes(keyBytes);
  return signer;
}

stakingRoutes.post(
  "/create",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, amount_lamports, validator_vote_address } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }

      if (!amount_lamports || typeof amount_lamports !== "number") {
        return res
          .status(400)
          .json({ error: "amount_lamports must be a number" });
      }

      if (amount_lamports < 1_000_000) {
        return res
          .status(400)
          .json({
            error: "Minimum stake amount is 1,000,000 lamports (0.001 SOL)",
          });
      }

      if (!validator_vote_address) {
        return res
          .status(400)
          .json({ error: "validator_vote_address is required" });
      }

      const wallet = walletDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        return res.status(403).json({ error: "Access denied" });
      }

      const signer = await getSignerForWallet(wallet_id, userId);

      const request: CreateStakeAccountRequest = {
        wallet_id,
        amount_lamports,
        validator_vote_address,
      };

      const result = await stakingManager.createAndDelegateStake(
        signer,
        wallet_id,
        userId,
        request,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "stake_created",
          wallet_id,
          stake_account: result.stake_account_address,
          amount_lamports,
          validator: validator_vote_address,
          signature: result.signature,
        },
      });

      res.status(201).json(result);
    } catch (error) {
      logger.error("Create stake error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to create stake",
      });
    }
  },
);

stakingRoutes.post("/deactivate", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { wallet_id, stake_account_address } = req.body;

    if (!wallet_id) {
      return res.status(400).json({ error: "wallet_id is required" });
    }

    if (!stake_account_address) {
      return res
        .status(400)
        .json({ error: "stake_account_address is required" });
    }

    const wallet = walletDatabase.getWallet(wallet_id);
    if (!wallet) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (wallet.user_id !== userId) {
      return res.status(403).json({ error: "Access denied" });
    }

    const signer = await getSignerForWallet(wallet_id, userId);

    const request: DeactivateStakeRequest = {
      wallet_id,
      stake_account_address,
    };

    const result = await stakingManager.deactivateStake(
      signer,
      wallet_id,
      userId,
      request,
    );

    auditLogger.log({
      timestamp: Date.now(),
      event_type: "TRANSFER_COMPLETED" as any,
      user_id: userId,
      success: true,
      details: {
        action: "stake_deactivated",
        wallet_id,
        stake_account: stake_account_address,
        signature: result.signature,
      },
    });

    res.json(result);
  } catch (error) {
    logger.error("Deactivate stake error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to deactivate stake",
    });
  }
});

stakingRoutes.post("/withdraw", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const {
      wallet_id,
      stake_account_address,
      recipient_address,
      amount_lamports,
    } = req.body;

    if (!wallet_id) {
      return res.status(400).json({ error: "wallet_id is required" });
    }

    if (!stake_account_address) {
      return res
        .status(400)
        .json({ error: "stake_account_address is required" });
    }

    if (!recipient_address) {
      return res.status(400).json({ error: "recipient_address is required" });
    }

    if (!amount_lamports || typeof amount_lamports !== "number") {
      return res
        .status(400)
        .json({ error: "amount_lamports must be a number" });
    }

    const wallet = walletDatabase.getWallet(wallet_id);
    if (!wallet) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (wallet.user_id !== userId) {
      return res.status(403).json({ error: "Access denied" });
    }

    const signer = await getSignerForWallet(wallet_id, userId);

    const request: WithdrawStakeRequest = {
      wallet_id,
      stake_account_address,
      recipient_address,
      amount_lamports,
    };

    const result = await stakingManager.withdrawStake(
      signer,
      wallet_id,
      userId,
      request,
    );

    auditLogger.log({
      timestamp: Date.now(),
      event_type: "TRANSFER_COMPLETED" as any,
      user_id: userId,
      success: true,
      details: {
        action: "stake_withdrawn",
        wallet_id,
        stake_account: stake_account_address,
        recipient: recipient_address,
        amount_lamports,
        signature: result.signature,
      },
    });

    res.json(result);
  } catch (error) {
    logger.error("Withdraw stake error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to withdraw stake",
    });
  }
});

stakingRoutes.get(
  "/account/:stake_account_address",
  authenticate,
  async (req: any, res) => {
    try {
      const { stake_account_address } = req.params;

      if (!stake_account_address) {
        return res
          .status(400)
          .json({ error: "stake_account_address is required" });
      }

      const info = await stakingManager.getStakeAccountInfo(
        stake_account_address,
      );

      res.json(info);
    } catch (error) {
      logger.error("Get stake account info error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get stake account info",
      });
    }
  },
);

stakingRoutes.get("/validators", authenticate, async (req: any, res) => {
  try {
    const validators = await stakingManager.getValidators();

    res.json({
      validators,
      total: validators.length,
      active: validators.filter((v) => v.epoch_vote_account).length,
      delinquent: validators.filter((v) => !v.epoch_vote_account).length,
    });
  } catch (error) {
    logger.error("Get validators error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get validators",
    });
  }
});

stakingRoutes.get(
  "/rewards/:stake_account_address",
  authenticate,
  async (req: any, res) => {
    try {
      const { stake_account_address } = req.params;
      const epochs = parseInt(req.query.epochs as string) || 5;

      if (!stake_account_address) {
        return res
          .status(400)
          .json({ error: "stake_account_address is required" });
      }

      if (epochs < 1 || epochs > 20) {
        return res
          .status(400)
          .json({ error: "epochs must be between 1 and 20" });
      }

      const rewards = await stakingManager.getStakingRewards(
        stake_account_address,
        epochs,
      );

      res.json(rewards);
    } catch (error) {
      logger.error("Get staking rewards error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get staking rewards",
      });
    }
  },
);

stakingRoutes.get("/user", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const stakes = await stakingManager.getUserStakes(userId);

    res.json({
      user_id: userId,
      stakes,
      total: stakes.length,
      active: stakes.filter((s) => s.status === "delegated").length,
      deactivating: stakes.filter((s) => s.status === "deactivated").length,
      withdrawn: stakes.filter((s) => s.status === "withdrawn").length,
    });
  } catch (error) {
    logger.error("Get user stakes error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get user stakes",
    });
  }
});

stakingRoutes.get("/wallet/:wallet_id", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { wallet_id } = req.params;

    const wallet = walletDatabase.getWallet(wallet_id);
    if (!wallet) {
      return res.status(404).json({ error: "Wallet not found" });
    }

    if (wallet.user_id !== userId) {
      return res.status(403).json({ error: "Access denied" });
    }

    const stakes = await stakingManager.getWalletStakes(wallet_id);

    res.json({
      wallet_id,
      stakes,
      total: stakes.length,
      active: stakes.filter((s) => s.status === "delegated").length,
      deactivating: stakes.filter((s) => s.status === "deactivated").length,
      withdrawn: stakes.filter((s) => s.status === "withdrawn").length,
    });
  } catch (error) {
    logger.error("Get wallet stakes error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get wallet stakes",
    });
  }
});
