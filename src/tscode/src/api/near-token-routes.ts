import express from "express";
import { nearTokenManager } from "../services/near/token.js";
import { nearDatabase } from "../services/database/near-database.js";
import { authenticate } from "../middleware/auth.js";
import { transferLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  SendNearTokensRequest,
  SendFungibleTokenRequest,
  RegisterTokenAccountRequest,
} from "../core/near-types.js";

export const nearTokenRoutes = express.Router();

nearTokenRoutes.post(
  "/send/near",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { from_wallet_id, receiver_id, amount } = req.body;

      if (!from_wallet_id) {
        return res.status(400).json({ error: "from_wallet_id is required" });
      }

      if (!receiver_id) {
        return res.status(400).json({ error: "receiver_id is required" });
      }

      if (!amount) {
        return res.status(400).json({ error: "amount is required" });
      }

      if (isNaN(parseFloat(amount)) || parseFloat(amount) <= 0) {
        return res
          .status(400)
          .json({ error: "amount must be a positive number" });
      }

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near token send NEAR",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: SendNearTokensRequest = {
        from_wallet_id,
        receiver_id,
        amount,
      };

      const result = await nearTokenManager.sendNear(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_send_near",
          from_account_id: result.from_account_id,
          receiver_id: result.receiver_id,
          amount: result.amount,
          token: result.token,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Send NEAR tokens error:", error);
      const errorMessage =
        error instanceof Error ? error.message : "Failed to send NEAR tokens";

      if (errorMessage.includes("Insufficient")) {
        return res.status(400).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearTokenRoutes.post(
  "/send/fungible",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        from_wallet_id,
        receiver_id,
        amount,
        token_type,
        contract_id,
        token_decimals,
        token_symbol,
        token_name,
      } = req.body;

      if (!from_wallet_id) {
        return res.status(400).json({ error: "from_wallet_id is required" });
      }

      if (!receiver_id) {
        return res.status(400).json({ error: "receiver_id is required" });
      }

      if (!amount) {
        return res.status(400).json({ error: "amount is required" });
      }

      if (isNaN(parseFloat(amount)) || parseFloat(amount) <= 0) {
        return res
          .status(400)
          .json({ error: "amount must be a positive number" });
      }

      if (!token_type) {
        return res
          .status(400)
          .json({ error: "token_type is required (USDT or custom)" });
      }

      if (!["USDT", "custom"].includes(token_type)) {
        return res
          .status(400)
          .json({ error: "token_type must be USDT or custom" });
      }

      if (token_type === "custom" && !contract_id) {
        return res
          .status(400)
          .json({ error: "contract_id is required for custom token" });
      }

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near token send fungible",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: SendFungibleTokenRequest = {
        from_wallet_id,
        receiver_id,
        amount,
        token_type,
        contract_id,
        token_decimals,
        token_symbol,
        token_name,
      };

      const result = await nearTokenManager.sendFungibleToken(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_send_fungible_token",
          from_account_id: result.from_account_id,
          receiver_id: result.receiver_id,
          amount: result.amount,
          token: result.token,
          token_contract: result.token_contract,
          transaction_hash: result.transaction_hash,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Send fungible token error:", error);
      const errorMessage =
        error instanceof Error
          ? error.message
          : "Failed to send fungible token";

      if (errorMessage.includes("Insufficient")) {
        return res.status(400).json({ error: errorMessage });
      }

      if (errorMessage.includes("not registered")) {
        return res.status(400).json({ error: errorMessage });
      }

      res.status(500).json({ error: errorMessage });
    }
  },
);

nearTokenRoutes.get(
  "/registration/:account_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const { account_id } = req.params;
      const {
        token_type,
        contract_id,
        token_decimals,
        token_symbol,
        token_name,
      } = req.query;

      if (!token_type) {
        return res
          .status(400)
          .json({
            error: "token_type query param is required (USDT or custom)",
          });
      }

      if (!["USDT", "custom"].includes(token_type as string)) {
        return res
          .status(400)
          .json({ error: "token_type must be USDT or custom" });
      }

      if (token_type === "custom" && !contract_id) {
        return res
          .status(400)
          .json({ error: "contract_id is required for custom token" });
      }

      const result = await nearTokenManager.checkTokenRegistration(
        account_id,
        token_type as "USDT" | "custom",
        contract_id as string | undefined,
        token_decimals ? parseInt(token_decimals as string) : undefined,
        token_symbol as string | undefined,
        token_name as string | undefined,
      );

      res.json(result);
    } catch (error) {
      logger.error("Check token registration error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to check registration",
      });
    }
  },
);

nearTokenRoutes.post(
  "/register",
  authenticate,
  transferLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const {
        from_wallet_id,
        account_id_to_register,
        token_type,
        contract_id,
      } = req.body;

      if (!from_wallet_id) {
        return res.status(400).json({ error: "from_wallet_id is required" });
      }

      if (!account_id_to_register) {
        return res
          .status(400)
          .json({ error: "account_id_to_register is required" });
      }

      if (!token_type) {
        return res
          .status(400)
          .json({ error: "token_type is required (USDT or custom)" });
      }

      if (!["USDT", "custom"].includes(token_type)) {
        return res
          .status(400)
          .json({ error: "token_type must be USDT or custom" });
      }

      if (token_type === "custom" && !contract_id) {
        return res
          .status(400)
          .json({ error: "contract_id is required for custom token" });
      }

      const wallet = nearDatabase.getWallet(from_wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Funding wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          "near token register account",
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const request: RegisterTokenAccountRequest = {
        from_wallet_id,
        account_id_to_register,
        token_type,
        contract_id,
      };

      const result = await nearTokenManager.registerTokenAccount(request);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "TRANSFER_COMPLETED" as any,
        user_id: userId,
        success: true,
        details: {
          action: "near_register_token_account",
          registered_account_id: result.registered_account_id,
          token_contract: result.token_contract,
          funded_by: result.funded_by,
        },
      });

      res.json(result);
    } catch (error) {
      logger.error("Register token account error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to register token account",
      });
    }
  },
);

nearTokenRoutes.get(
  "/balance/:wallet_id",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { wallet_id } = req.params;
      const {
        token_type,
        contract_id,
        token_decimals,
        token_symbol,
        token_name,
      } = req.query;

      if (!token_type) {
        return res
          .status(400)
          .json({
            error: "token_type query param is required (USDT or custom)",
          });
      }

      if (!["USDT", "custom"].includes(token_type as string)) {
        return res
          .status(400)
          .json({ error: "token_type must be USDT or custom" });
      }

      if (token_type === "custom" && !contract_id) {
        return res
          .status(400)
          .json({ error: "contract_id is required for custom token" });
      }

      const wallet = nearDatabase.getWallet(wallet_id);
      if (!wallet) {
        return res.status(404).json({ error: "Wallet not found" });
      }

      if (wallet.user_id !== userId) {
        auditLogger.logUnauthorizedAccess(
          userId,
          req.ip || "unknown",
          `near/tokens/balance/${wallet_id}`,
        );
        return res
          .status(403)
          .json({ error: "Unauthorized: wallet does not belong to user" });
      }

      const result = await nearTokenManager.getTokenBalance(
        wallet_id,
        token_type as "USDT" | "custom",
        contract_id as string | undefined,
        token_decimals ? parseInt(token_decimals as string) : undefined,
        token_symbol as string | undefined,
        token_name as string | undefined,
      );

      res.json(result);
    } catch (error) {
      logger.error("Get token balance error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get token balance",
      });
    }
  },
);
