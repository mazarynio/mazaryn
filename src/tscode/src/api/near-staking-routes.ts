import express from "express";
import { nearStakingManager } from "../services/near/staking.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  StakeNearRequest,
  UnstakeNearRequest,
} from "../core/near-types.js";

export const nearStakingRoutes = express.Router();

nearStakingRoutes.post(
  "/stake",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, amount_near, validator_public_key } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!amount_near) {
        return res.status(400).json({ error: "amount_near is required" });
      }
      if (isNaN(parseFloat(amount_near)) || parseFloat(amount_near) <= 0) {
        return res
          .status(400)
          .json({ error: "amount_near must be a positive number" });
      }
      if (!validator_public_key) {
        return res
          .status(400)
          .json({ error: "validator_public_key is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/staking/stake",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: StakeNearRequest = {
        wallet_id,
        amount_near,
        validator_public_key,
      };
      const result = await nearStakingManager.stake(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_stake",
          account_id: result.account_id,
          amount_near: result.amount_near,
          validator_public_key: result.validator_public_key,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Stake error:", error);
      const msg = error instanceof Error ? error.message : "Failed to stake";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearStakingRoutes.post(
  "/unstake",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id, amount_near, validator_public_key } = req.body;

      if (!wallet_id) {
        return res.status(400).json({ error: "wallet_id is required" });
      }
      if (!amount_near) {
        return res.status(400).json({ error: "amount_near is required" });
      }
      if (!validator_public_key) {
        return res
          .status(400)
          .json({ error: "validator_public_key is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near/staking/unstake",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: UnstakeNearRequest = {
        wallet_id,
        amount_near,
        validator_public_key,
      };
      const result = await nearStakingManager.unstake(request, userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_unstake",
          account_id: result.account_id,
          amount_near: result.amount_near,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Unstake error:", error);
      const msg = error instanceof Error ? error.message : "Failed to unstake";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearStakingRoutes.get(
  "/balance/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { validator_account_id } = req.query;

      if (!validator_account_id) {
        return res
          .status(400)
          .json({ error: "validator_account_id query param is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/staking/balance/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const [staked, unstaked] = await Promise.all([
        nearStakingManager.getStakedBalance(
          wallet_id,
          userId,
          validator_account_id as string,
        ),
        nearStakingManager.getUnstakedBalance(
          wallet_id,
          userId,
          validator_account_id as string,
        ),
      ]);

      res.json({
        wallet_id,
        account_id: wallet.account_id,
        validator_account_id,
        staked_yocto: staked.staked_yocto,
        staked_near: staked.staked_near,
        unstaked_yocto: unstaked.unstaked_yocto,
        unstaked_near: unstaked.unstaked_near,
      });
    } catch (error) {
      logger.error("Get staking balance error:", error);
      const msg =
        error instanceof Error
          ? error.message
          : "Failed to get staking balance";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearStakingRoutes.get(
  "/available/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { validator_account_id } = req.query;

      if (!validator_account_id) {
        return res
          .status(400)
          .json({ error: "validator_account_id query param is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/staking/available/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearStakingManager.isUnstakedBalanceAvailable(
        wallet_id,
        userId,
        validator_account_id as string,
      );

      res.json(result);
    } catch (error) {
      logger.error("Check unstake available error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to check availability";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearStakingRoutes.post(
  "/withdraw/:wallet_id",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const { validator_account_id } = req.body;

      if (!validator_account_id) {
        return res
          .status(400)
          .json({ error: "validator_account_id is required" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }
      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/staking/withdraw/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearStakingManager.withdrawStake(
        wallet_id,
        userId,
        validator_account_id,
      );

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_withdraw_stake",
          account_id: result.account_id,
          validator_account_id: result.validator_account_id,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Withdraw stake error:", error);
      const msg =
        error instanceof Error ? error.message : "Failed to withdraw stake";
      if (msg === "Unauthorized") return res.status(403).json({ error: msg });
      if (msg === "Wallet not found")
        return res.status(404).json({ error: msg });
      res.status(500).json({ error: msg });
    }
  },
);

nearStakingRoutes.get(
  "/validator/:validator_account_id",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { validator_account_id } = req.params;

      const result =
        await nearStakingManager.getValidatorInfo(validator_account_id);
      res.json(result);
    } catch (error) {
      logger.error("Get validator info error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get validator info",
      });
    }
  },
);

nearStakingRoutes.get("/epoch-prices", generalLimiter, async (_req, res) => {
  try {
    const result = await nearStakingManager.getCurrentEpochSeatPrice();
    res.json(result);
  } catch (error) {
    logger.error("Get epoch prices error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get epoch prices",
    });
  }
});
