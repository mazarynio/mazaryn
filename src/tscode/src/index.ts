import express from "express";
import { walletRoutes } from "./api/routes.js";
import { logger } from "./core/logger.js";
import { config } from "./config/index.js";

const app = express();

app.use(express.json({ limit: "10mb" }));

app.use("/wallet", walletRoutes);

app.get("/health", (req, res) => {
  res.json({
    status: "healthy",
    timestamp: Date.now(),
    service: "solana_wallet",
  });
});

app.listen(config.port, () => {
  logger.info(`Solana Wallet Service running on port ${config.port}`);
});
