import express from "express";
import { authService } from "../services/security/auth.js";
import { logger } from "../core/logger.js";

export interface AuthRequest extends express.Request {
  user?: {
    user_id: string;
    username: string;
  };
}

export function authenticate(
  req: express.Request,
  res: express.Response,
  next: express.NextFunction,
) {
  try {
    const authReq = req as AuthRequest;
    const authHeader = req.headers.authorization;

    if (!authHeader) {
      return res.status(401).json({ error: "No authorization token provided" });
    }

    const parts = authHeader.split(" ");
    if (parts.length !== 2 || parts[0] !== "Bearer") {
      return res
        .status(401)
        .json({ error: "Invalid authorization format. Use: Bearer <token>" });
    }

    const token = parts[1];
    const decoded = authService.verifyToken(token);

    authReq.user = {
      user_id: decoded.user_id,
      username: decoded.username,
    };

    next();
  } catch (error) {
    logger.error("Authentication failed:", error);
    return res.status(401).json({
      error: "Invalid or expired token",
      message: error instanceof Error ? error.message : "Authentication failed",
    });
  }
}

export function optionalAuth(
  req: express.Request,
  res: express.Response,
  next: express.NextFunction,
) {
  try {
    const authReq = req as AuthRequest;
    const authHeader = req.headers.authorization;

    if (!authHeader) {
      return next();
    }

    const parts = authHeader.split(" ");
    if (parts.length !== 2 || parts[0] !== "Bearer") {
      return next();
    }

    const token = parts[1];
    const decoded = authService.verifyToken(token);

    authReq.user = {
      user_id: decoded.user_id,
      username: decoded.username,
    };

    next();
  } catch (error) {
    next();
  }
}
