import express from "express";
import { authService } from "../services/security/auth.js";
import { walletDatabase } from "../services/database/database.js";
import { authenticate } from "../middleware/auth.js";
import { authLimiter, generalLimiter } from "../middleware/rate-limiter.js";
import { auditLogger } from "../services/security/audit-logger.js";
import { logger } from "../core/logger.js";
import type {
  RegisterRequest,
  LoginRequest,
} from "../services/security/auth.js";

export const authRoutes = express.Router();

function getSecurityRecommendations(
  user: any,
  passwordAge: number | null,
): string[] {
  const recommendations: string[] = [];

  if (user.two_fa_enabled !== 1) {
    recommendations.push("Enable 2FA for enhanced account security");
  }

  if (passwordAge && passwordAge > 90) {
    recommendations.push(
      "Consider changing your password (it has been over 90 days)",
    );
  }

  if (!user.email) {
    recommendations.push("Add an email address for account recovery");
  }

  return recommendations;
}

authRoutes.post("/register", authLimiter, async (req, res) => {
  try {
    const request = req.body as RegisterRequest;
    const ipAddress = req.ip || req.socket.remoteAddress || "unknown";

    if (!request.username || !request.password) {
      return res
        .status(400)
        .json({ error: "Username and password are required" });
    }

    const result = await authService.register(request, ipAddress);
    res.status(201).json(result);
  } catch (error) {
    logger.error("Registration error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Registration failed";

    if (errorMessage === "Username already exists") {
      return res.status(409).json({ error: errorMessage });
    }

    if (
      errorMessage.includes("Username must be") ||
      errorMessage.includes("Password must be") ||
      errorMessage.includes("Invalid email")
    ) {
      return res.status(400).json({ error: errorMessage });
    }

    res.status(500).json({ error: errorMessage });
  }
});

authRoutes.post("/login", authLimiter, async (req, res) => {
  try {
    const ipAddress = req.ip || req.socket.remoteAddress || "unknown";
    const userAgent = req.headers["user-agent"];

    const request: LoginRequest = {
      ...req.body,
      ip_address: ipAddress,
      user_agent: userAgent,
    };

    if (!request.username || !request.password) {
      return res
        .status(400)
        .json({ error: "Username and password are required" });
    }

    const result = await authService.login(request);

    if (result.requires_2fa) {
      return res.status(200).json({
        requires_2fa: true,
        message: "Please provide your 2FA token",
      });
    }

    res.json(result);
  } catch (error) {
    logger.error("Login error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Login failed";

    if (errorMessage === "Invalid username or password") {
      return res.status(401).json({ error: errorMessage });
    }

    if (errorMessage.includes("locked") || errorMessage.includes("blocked")) {
      return res.status(403).json({ error: errorMessage });
    }

    if (errorMessage.includes("2FA")) {
      return res.status(401).json({ error: errorMessage });
    }

    res.status(500).json({ error: errorMessage });
  }
});

authRoutes.post("/logout", authenticate, async (req: any, res) => {
  try {
    const sessionId = req.user.session_id;

    if (!sessionId) {
      return res.status(400).json({ error: "Invalid session" });
    }

    await authService.logout(sessionId);

    auditLogger.log({
      timestamp: Date.now(),
      event_type: "USER_LOGOUT" as any,
      user_id: req.user.user_id,
      username: req.user.username,
      success: true,
    });

    res.json({ success: true, message: "Logged out successfully" });
  } catch (error) {
    logger.error("Logout error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Logout failed",
    });
  }
});

authRoutes.post("/logout-all", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;

    await authService.logoutAllSessions(userId);

    auditLogger.log({
      timestamp: Date.now(),
      event_type: "USER_LOGOUT" as any,
      user_id: userId,
      username: req.user.username,
      success: true,
      details: { logout_all: true },
    });

    res.json({
      success: true,
      message: "All sessions logged out successfully",
    });
  } catch (error) {
    logger.error("Logout all error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Logout failed",
    });
  }
});

authRoutes.post("/verify", async (req, res) => {
  try {
    const { token } = req.body;

    if (!token) {
      return res.status(400).json({ error: "Token is required" });
    }

    const decoded = authService.verifyToken(token);
    res.json({
      valid: true,
      user_id: decoded.user_id,
      username: decoded.username,
      session_id: decoded.session_id,
      expires_at: decoded.exp,
    });
  } catch (error) {
    logger.error("Token verification error:", error);
    res.status(401).json({ valid: false, error: "Invalid or expired token" });
  }
});

authRoutes.get("/sessions", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;
    const sessions = authService.getUserSessions(userId);

    res.json({
      user_id: userId,
      sessions,
      total: sessions.length,
    });
  } catch (error) {
    logger.error("Get sessions error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Failed to get sessions",
    });
  }
});

authRoutes.delete(
  "/sessions/:session_id",
  authenticate,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const sessionId = req.params.session_id;

      const sessions = walletDatabase.getUserSessions(userId);
      const session = sessions.find((s: any) => s.session_id === sessionId);

      if (!session) {
        return res.status(404).json({ error: "Session not found" });
      }

      walletDatabase.invalidateSession(sessionId);

      res.json({ success: true, message: "Session terminated successfully" });
    } catch (error) {
      logger.error("Terminate session error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to terminate session",
      });
    }
  },
);

authRoutes.get("/me", authenticate, async (req: any, res) => {
  try {
    const userId = req.user.user_id;

    const user = walletDatabase.getUser(userId);
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    res.json({
      user_id: user.user_id,
      username: user.username,
      email: user.email,
      created_at: user.created_at,
      last_login: user.last_login,
      two_fa_enabled: user.two_fa_enabled === 1,
    });
  } catch (error) {
    logger.error("Get user info error:", error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Failed to get user info",
    });
  }
});

authRoutes.get(
  "/security-status",
  authenticate,
  generalLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;

      const user = walletDatabase.getUser(userId);
      if (!user) {
        return res.status(404).json({ error: "User not found" });
      }

      const recentAttempts = walletDatabase.getRecentLoginAttempts(
        user.username,
        60,
      );
      const failedAttempts = recentAttempts.filter(
        (a: any) => a.success === 0,
      ).length;

      const sessions = walletDatabase.getUserSessions(userId);

      const passwordAge = user.password_changed_at
        ? Math.floor(
            (Date.now() - user.password_changed_at) / (24 * 60 * 60 * 1000),
          )
        : null;

      res.json({
        two_fa_enabled: user.two_fa_enabled === 1,
        active_sessions: sessions.length,
        failed_login_attempts_last_hour: failedAttempts,
        account_locked: user.is_locked === 1,
        password_age_days: passwordAge,
        last_login: user.last_login,
        security_recommendations: getSecurityRecommendations(user, passwordAge),
      });
    } catch (error) {
      logger.error("Get security status error:", error);
      res.status(500).json({
        error:
          error instanceof Error
            ? error.message
            : "Failed to get security status",
      });
    }
  },
);

authRoutes.post(
  "/change-password",
  authenticate,
  authLimiter,
  async (req: any, res) => {
    try {
      const userId = req.user.user_id;
      const { current_password, new_password } = req.body;
      const ip = req.ip || req.socket.remoteAddress || "unknown";

      if (!current_password || !new_password) {
        return res
          .status(400)
          .json({ error: "current_password and new_password are required" });
      }

      if (new_password.length < 8) {
        return res
          .status(400)
          .json({ error: "New password must be at least 8 characters" });
      }

      if (!/(?=.*[a-z])(?=.*[A-Z])(?=.*\d)/.test(new_password)) {
        return res
          .status(400)
          .json({
            error: "New password must contain uppercase, lowercase, and number",
          });
      }

      const user = walletDatabase.getUser(userId);
      if (!user) {
        return res.status(404).json({ error: "User not found" });
      }

      const { encryptionService } = await import(
        "../services/security/encryption.js"
      );
      const passwordValid = encryptionService.verifyPassword(
        current_password,
        user.password_hash,
      );

      if (!passwordValid) {
        auditLogger.logSuspiciousActivity(
          userId,
          ip,
          "Failed password change attempt",
          {},
        );
        return res.status(401).json({ error: "Current password is incorrect" });
      }

      const newHash = encryptionService.hashPassword(new_password);

      walletDatabase.updateUser(userId, {
        password_hash: newHash,
        password_changed_at: Date.now(),
      });

      await authService.logoutAllSessions(userId);

      auditLogger.log({
        timestamp: Date.now(),
        event_type: "PASSWORD_CHANGED" as any,
        user_id: userId,
        ip_address: ip,
        success: true,
        details: {},
      });

      res.json({
        success: true,
        message:
          "Password changed successfully. All sessions have been terminated.",
      });
    } catch (error) {
      logger.error("Change password error:", error);
      res.status(500).json({
        error:
          error instanceof Error ? error.message : "Failed to change password",
      });
    }
  },
);
