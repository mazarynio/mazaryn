import express from "express";
import { aptosStakingManager } from "../services/aptos/staking.js";
import { aptosDatabase } from "../services/database/aptos-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  AptosStakeRequest,
  AptosUnstakeRequest,
  AptosWithdrawStakeRequest,
  AptosGetStakeInfoRequest,
  AptosReactivateStakeRequest,
} from "../core/aptos-types.js";

export const aptosStakingRoutes = express.Router();

aptosStakingRoutes.post(
  "/add-stake",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { delegator_address, pool_address, amount_octas, network } =
        req.body;

      if (!delegator_address) {
        return res.status(400).json({ error: "delegator_address is required" });
      }
      if (!pool_address) {
        return res.status(400).json({ error: "pool_address is required" });
      }
      if (amount_octas === undefined || amount_octas <= 0) {
        return res
          .status(400)
          .json({ error: "amount_octas must be a positive number" });
      }

      const wallet = aptosDatabase.getWalletByAddress(delegator_address);
      if (!wallet) {
        return res.status(404).json({ error: "Delegator wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/staking/add-stake",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosStakeRequest = {
        delegator_address,
        pool_address,
        amount_octas,
        network,
      };

      const result = await aptosStakingManager.addStake(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_stake_added",
          delegator_address,
          pool_address,
          amount_octas,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos add stake error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to add stake",
      });
    }
  },
);

aptosStakingRoutes.post(
  "/unlock",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { delegator_address, pool_address, amount_octas, network } =
        req.body;

      if (!delegator_address) {
        return res.status(400).json({ error: "delegator_address is required" });
      }
      if (!pool_address) {
        return res.status(400).json({ error: "pool_address is required" });
      }
      if (amount_octas === undefined || amount_octas <= 0) {
        return res
          .status(400)
          .json({ error: "amount_octas must be a positive number" });
      }

      const wallet = aptosDatabase.getWalletByAddress(delegator_address);
      if (!wallet) {
        return res.status(404).json({ error: "Delegator wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/staking/unlock",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosUnstakeRequest = {
        delegator_address,
        pool_address,
        amount_octas,
        network,
      };

      const result = await aptosStakingManager.unlockStake(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_stake_unlocked",
          delegator_address,
          pool_address,
          amount_octas,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos unlock stake error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to unlock stake",
      });
    }
  },
);

aptosStakingRoutes.post(
  "/withdraw",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { delegator_address, pool_address, network } = req.body;

      if (!delegator_address) {
        return res.status(400).json({ error: "delegator_address is required" });
      }
      if (!pool_address) {
        return res.status(400).json({ error: "pool_address is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(delegator_address);
      if (!wallet) {
        return res.status(404).json({ error: "Delegator wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/staking/withdraw",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosWithdrawStakeRequest = {
        delegator_address,
        pool_address,
        network,
      };

      const result = await aptosStakingManager.withdrawStake(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_stake_withdrawn",
          delegator_address,
          pool_address,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos withdraw stake error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to withdraw stake",
      });
    }
  },
);

aptosStakingRoutes.post(
  "/reactivate",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { delegator_address, pool_address, amount_octas, network } =
        req.body;

      if (!delegator_address) {
        return res.status(400).json({ error: "delegator_address is required" });
      }
      if (!pool_address) {
        return res.status(400).json({ error: "pool_address is required" });
      }
      if (amount_octas === undefined || amount_octas <= 0) {
        return res
          .status(400)
          .json({ error: "amount_octas must be a positive number" });
      }

      const wallet = aptosDatabase.getWalletByAddress(delegator_address);
      if (!wallet) {
        return res.status(404).json({ error: "Delegator wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/staking/reactivate",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosReactivateStakeRequest = {
        delegator_address,
        pool_address,
        amount_octas,
        network,
      };

      const result = await aptosStakingManager.reactivateStake(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED",
        user_id: userId,
        success: true,
        details: {
          action: "aptos_stake_reactivated",
          delegator_address,
          pool_address,
          amount_octas,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Aptos reactivate stake error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to reactivate stake",
      });
    }
  },
);

aptosStakingRoutes.get(
  "/stake-info",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { delegator_address, pool_address, network } = req.query;

      if (!delegator_address) {
        return res
          .status(400)
          .json({ error: "delegator_address query parameter is required" });
      }
      if (!pool_address) {
        return res
          .status(400)
          .json({ error: "pool_address query parameter is required" });
      }

      const wallet = aptosDatabase.getWalletByAddress(
        delegator_address as string,
      );
      if (!wallet) {
        return res.status(404).json({ error: "Delegator wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "aptos/staking/stake-info",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: AptosGetStakeInfoRequest = {
        delegator_address: delegator_address as string,
        pool_address: pool_address as string,
        network: network as string | undefined,
      };

      const result = await aptosStakingManager.getStakeInfo(request);
      res.json(result);
    } catch (error) {
      logger.error("Get Aptos stake info error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to get stake info",
      });
    }
  },
);

aptosStakingRoutes.get("/pools", generalLimiter, async (req: any, res) => {
  try {
    const { network } = req.query;
    const result = await aptosStakingManager.getDelegationPools(
      network as string | undefined,
    );
    res.json(result);
  } catch (error) {
    logger.error("Get Aptos delegation pools error:", error);
    res.status(500).json({
      error:
        error instanceof Error
          ? error.message
          : "Failed to get delegation pools",
    });
  }
});

aptosStakingRoutes.get(
  "/pools/:pool_address",
  generalLimiter,
  async (req: any, res) => {
    try {
      const { pool_address } = req.params;
      const { network } = req.query;

      const result = await aptosStakingManager.getPoolStakeInfo(
        pool_address,
        network as string | undefined,
      );
      res.json(result);
    } catch (error) {
      logger.error("Get Aptos pool stake info error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to get pool info";
      if (errorMessage.includes("does not exist")) {
        return res.status(404).json({ error: errorMessage });
      }
      res.status(500).json({ error: errorMessage });
    }
  },
);

aptosStakingRoutes.get(
  "/activities/:delegator_address",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { delegator_address } = req.params;
      const { network } = req.query;

      const wallet = aptosDatabase.getWalletByAddress(delegator_address);
      if (!wallet) {
        return res.status(404).json({ error: "Delegator wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `aptos/staking/activities/${delegator_address}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await aptosStakingManager.getStakingActivities(
        delegator_address,
        network as string | undefined,
      );

      res.json({
        delegator_address,
        total: result.length,
        activities: result,
      });
    } catch (error) {
      logger.error("Get Aptos staking activities error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get staking activities",
      });
    }
  },
);
