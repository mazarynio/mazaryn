import express from "express";
import { walletManager } from "../services/solana/wallet.js";
import { logger } from "../core/logger.js";
import type {
  CreateWalletRequest,
  ImportWalletRequest,
  GetBalanceRequest,
  TransferRequest,
  ErrorResponse,
} from "../core/types.js";

export const walletRoutes = express.Router();

walletRoutes.post("/create", async (req, res) => {
  try {
    const request = req.body as CreateWalletRequest;
    logger.info("Create wallet request received for user:", request.user_id);

    const result = await walletManager.createWallet(request);
    res.json(result);
  } catch (error) {
    logger.error("Create wallet error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.post("/import", async (req, res) => {
  try {
    const request = req.body as ImportWalletRequest;
    logger.info("Import wallet request received for user:", request.user_id);

    const result = await walletManager.importWallet(request);
    res.json(result);
  } catch (error) {
    logger.error("Import wallet error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.post("/balance", async (req, res) => {
  try {
    const request = req.body as GetBalanceRequest;
    logger.info("Get balance request for:", request.public_key);

    const result = await walletManager.getBalance(request.public_key);
    res.json(result);
  } catch (error) {
    logger.error("Get balance error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.post("/transfer", async (req, res) => {
  try {
    const request = req.body as TransferRequest;
    logger.info("Transfer request from wallet:", request.from_wallet_id);

    const result = await walletManager.transfer(request);
    res.json(result);
  } catch (error) {
    logger.error("Transfer error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.get("/transaction/:signature", async (req, res) => {
  try {
    const signature = req.params.signature;
    logger.info("Get transaction request for:", signature);

    const result = await walletManager.getTransaction(signature);
    res.json(result);
  } catch (error) {
    logger.error("Get transaction error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(404).json(errorResponse);
  }
});

walletRoutes.get("/info/:wallet_id", async (req, res) => {
  try {
    const walletId = req.params.wallet_id;
    logger.info("Get wallet info request for:", walletId);

    const result = await walletManager.getWalletInfo(walletId);
    if (!result) {
      const errorResponse: ErrorResponse = { error: "Wallet not found" };
      return res.status(404).json(errorResponse);
    }

    res.json(result);
  } catch (error) {
    logger.error("Get wallet info error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.get("/user/:user_id", async (req, res) => {
  try {
    const userId = req.params.user_id;
    logger.info("Get user wallets request for:", userId);

    const result = await walletManager.getUserWallets(userId);
    res.json({ user_id: userId, wallets: result });
  } catch (error) {
    logger.error("Get user wallets error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.post("/export/:wallet_id", async (req, res) => {
  try {
    const walletId = req.params.wallet_id;
    logger.info("Export private key request for:", walletId);

    const privateKey = await walletManager.exportPrivateKey(walletId);
    res.json({ wallet_id: walletId, private_key: privateKey });
  } catch (error) {
    logger.error("Export private key error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});

walletRoutes.delete("/:wallet_id", async (req, res) => {
  try {
    const walletId = req.params.wallet_id;
    const userId = req.body.user_id;

    logger.info("Delete wallet request:", walletId);

    const success = walletManager.deleteWallet(walletId, userId);
    if (!success) {
      const errorResponse: ErrorResponse = {
        error: "Wallet not found or unauthorized",
      };
      return res.status(404).json(errorResponse);
    }

    res.json({ status: "deleted", wallet_id: walletId });
  } catch (error) {
    logger.error("Delete wallet error:", error);
    const errorResponse: ErrorResponse = {
      error: error instanceof Error ? error.message : "Unknown error",
      details: error,
    };
    res.status(500).json(errorResponse);
  }
});
