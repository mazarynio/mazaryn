import express from "express";
import { aptosConnection } from "../services/aptos/connection.js";
import { generalLimiter } from "../middleware/rate-limiter.js";
import { logger } from "../core/logger.js";

export const aptosLedgerRoutes = express.Router();

aptosLedgerRoutes.get("/ledger", generalLimiter, async (req: any, res) => {
  try {
    const { network } = req.query;
    const info = await aptosConnection.getLedgerInfo(network as string);
    res.json(info);
  } catch (error) {
    logger.error("Get Aptos ledger info error:", error);
    res.status(500).json({
      error:
        error instanceof Error ? error.message : "Failed to get ledger info",
    });
  }
});

aptosLedgerRoutes.get(
  "/blocks/by-height/:height",
  generalLimiter,
  async (req: any, res) => {
    try {
      const height = parseInt(req.params.height, 10);
      const { network } = req.query;

      if (isNaN(height) || height < 0) {
        return res
          .status(400)
          .json({ error: "height must be a non-negative integer" });
      }

      const block = await aptosConnection.getBlockByHeight(
        height,
        network as string,
      );
      res.json(block);
    } catch (error) {
      logger.error("Get Aptos block error:", error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Failed to get block",
      });
    }
  },
);
