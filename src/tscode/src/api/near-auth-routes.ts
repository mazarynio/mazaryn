import express from "express";
import { nearAuthManager } from "../services/near/auth.js";
import { generalLimiter } from "../middleware/rate-limiter.js";
import { logger } from "../core/logger.js";
import type { VerifyNep413MessageRequest } from "../core/near-types.js";

export const nearAuthRoutes = express.Router();

nearAuthRoutes.post("/verify", generalLimiter, async (req: any, res) => {
  try {
    const {
      signer_account_id,
      signer_public_key,
      signature_base64,
      message,
      recipient,
      nonce_hex,
    } = req.body;

    if (!signer_account_id) {
      return res.status(400).json({ error: "signer_account_id is required" });
    }
    if (!signer_public_key) {
      return res.status(400).json({ error: "signer_public_key is required" });
    }
    if (!signature_base64) {
      return res.status(400).json({ error: "signature_base64 is required" });
    }
    if (!message) {
      return res.status(400).json({ error: "message is required" });
    }
    if (!recipient) {
      return res.status(400).json({ error: "recipient is required" });
    }
    if (!nonce_hex) {
      return res.status(400).json({ error: "nonce_hex is required" });
    }

    const request: VerifyNep413MessageRequest = {
      signer_account_id,
      signer_public_key,
      signature_base64,
      message,
      recipient,
      nonce_hex,
    };

    const result = await nearAuthManager.verifyNep413Message(request);

    if (!result.valid) {
      return res.status(401).json(result);
    }

    res.json(result);
  } catch (error) {
    logger.error("NEP-413 verify error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to verify message",
    });
  }
});
