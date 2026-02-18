import rateLimit from "express-rate-limit";
import { config } from "../config/index.js";
import { auditLogger } from "../services/security/audit-logger.js";

export const generalLimiter = rateLimit({
  windowMs: config.rateLimitWindowMs,
  max: config.rateLimitMaxRequests,
  message: "Too many requests from this IP, please try again later",
  standardHeaders: true,
  legacyHeaders: false,
  handler: (req, res) => {
    const ip = req.ip || req.socket.remoteAddress || "unknown";
    auditLogger.logRateLimitExceeded(ip, req.path);
    res.status(429).json({
      error: "Too many requests",
      message: "Please try again later",
      retry_after: config.rateLimitWindowMs / 1000,
    });
  },
});

export const authLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 10,
  message: "Too many authentication attempts, please try again later",
  standardHeaders: true,
  legacyHeaders: false,
  skipSuccessfulRequests: false,
  handler: (req, res) => {
    const ip = req.ip || req.socket.remoteAddress || "unknown";
    auditLogger.logRateLimitExceeded(ip, req.path);
    res.status(429).json({
      error: "Too many authentication attempts",
      message: "Account temporarily locked. Please try again in 15 minutes",
    });
  },
});

export const transferLimiter = rateLimit({
  windowMs: 60 * 1000,
  max: 10,
  message: "Too many transfer requests, please slow down",
  standardHeaders: true,
  legacyHeaders: false,
  handler: (req, res) => {
    const ip = req.ip || req.socket.remoteAddress || "unknown";
    auditLogger.logRateLimitExceeded(ip, req.path);
    res.status(429).json({
      error: "Too many transfer requests",
      message: "Please wait a moment before making another transfer",
    });
  },
});

export const exportKeyLimiter = rateLimit({
  windowMs: 60 * 60 * 1000,
  max: 5,
  message: "Too many private key export attempts",
  standardHeaders: true,
  legacyHeaders: false,
  handler: (req, res) => {
    const ip = req.ip || req.socket.remoteAddress || "unknown";
    auditLogger.logRateLimitExceeded(ip, req.path);
    auditLogger.logSuspiciousActivity(
      undefined,
      ip,
      "Excessive private key export attempts",
      { path: req.path },
    );
    res.status(429).json({
      error: "Too many private key export attempts",
      message:
        "This activity has been logged. Please contact support if you need assistance",
    });
  },
});
