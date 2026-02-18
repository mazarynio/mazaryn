import express from "express";
import { twoFactorService } from "../services/security/two-factor.js";
import { walletDatabase } from "../services/database/database.js";
import { authenticate } from "../middleware/auth.js";
import { logger } from "../core/logger.js";

export const twoFARoutes = express.Router();

twoFARoutes.post("/setup", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const username = req.user.username;

    logger.info("2FA setup request for user:", userId);

    const user = walletDatabase.getUser(userId);
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    if (user.two_fa_enabled) {
      return res.status(400).json({ error: "2FA is already enabled" });
    }

    const setup = await twoFactorService.setupTwoFactor(userId, username);

    res.json({
      secret: setup.secret,
      qr_code: setup.qr_code,
      manual_entry_key: setup.manual_entry_key,
      message:
        "Scan the QR code with your authenticator app, then verify with a token to enable 2FA",
    });
  } catch (error) {
    logger.error("2FA setup error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Failed to setup 2FA",
    });
  }
});

twoFARoutes.post("/enable", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const username = req.user.username;
    const { secret, token } = req.body;

    if (!secret || !token) {
      return res.status(400).json({ error: "Secret and token are required" });
    }

    logger.info("2FA enable request for user:", userId);

    const user = walletDatabase.getUser(userId);
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    if (user.two_fa_enabled) {
      return res.status(400).json({ error: "2FA is already enabled" });
    }

    const enabled = await twoFactorService.enableTwoFactor(
      userId,
      username,
      secret,
      token,
    );

    if (!enabled) {
      return res.status(400).json({ error: "Invalid verification token" });
    }

    const backupCodes = twoFactorService.generateBackupCodes();

    res.json({
      success: true,
      message: "2FA has been enabled successfully",
      backup_codes: backupCodes,
      warning:
        "Save these backup codes in a secure location. They can be used to access your account if you lose your authenticator device.",
    });
  } catch (error) {
    logger.error("2FA enable error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Failed to enable 2FA",
    });
  }
});

twoFARoutes.post("/disable", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const username = req.user.username;
    const { token } = req.body;

    if (!token) {
      return res.status(400).json({ error: "Verification token is required" });
    }

    logger.info("2FA disable request for user:", userId);

    const user = walletDatabase.getUser(userId);
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    if (!user.two_fa_enabled) {
      return res.status(400).json({ error: "2FA is not enabled" });
    }

    const disabled = await twoFactorService.disableTwoFactor(
      userId,
      username,
      token,
    );

    if (!disabled) {
      return res.status(400).json({ error: "Invalid verification token" });
    }

    res.json({
      success: true,
      message: "2FA has been disabled successfully",
    });
  } catch (error) {
    logger.error("2FA disable error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Failed to disable 2FA",
    });
  }
});

twoFARoutes.post("/verify", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const { token } = req.body;

    if (!token) {
      return res.status(400).json({ error: "Verification token is required" });
    }

    const user = walletDatabase.getUser(userId);
    if (!user || !user.two_fa_enabled || !user.two_fa_secret) {
      return res.status(400).json({ error: "2FA is not enabled" });
    }

    const valid = twoFactorService.verifyTwoFactorToken(
      user.two_fa_secret,
      token,
    );

    res.json({
      valid,
      message: valid ? "Token is valid" : "Token is invalid",
    });
  } catch (error) {
    logger.error("2FA verify error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Failed to verify token",
    });
  }
});

twoFARoutes.get("/status", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;

    const user = walletDatabase.getUser(userId);
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    res.json({
      enabled: user.two_fa_enabled === 1,
      message: user.two_fa_enabled ? "2FA is enabled" : "2FA is disabled",
    });
  } catch (error) {
    logger.error("2FA status error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get 2FA status",
    });
  }
});
