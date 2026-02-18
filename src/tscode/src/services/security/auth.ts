import jwt from "jsonwebtoken";
import crypto from "crypto";
import { v4 as uuidv4 } from "uuid";
import { config } from "../../config/index.js";
import { walletDatabase } from "../database/database.js";
import { encryptionService } from "./encryption.js";
import { auditLogger } from "./audit-logger.js";
import { twoFactorService } from "./two-factor.js";
import { logger } from "../../core/logger.js";

export interface JWTPayload {
  user_id: string;
  username: string;
  session_id: string;
  iat: number;
  exp: number;
}

export interface RegisterRequest {
  username: string;
  password: string;
  email?: string;
}

export interface LoginRequest {
  username: string;
  password: string;
  two_fa_token?: string;
  ip_address?: string;
  user_agent?: string;
}

export interface AuthResponse {
  user_id: string;
  username: string;
  token: string;
  expires_in: number;
  requires_2fa?: boolean;
  session_id: string;
}

const TOKEN_EXPIRY = 24 * 60 * 60;

export class AuthService {
  constructor() {
    setInterval(
      () => {
        walletDatabase.cleanupExpiredSessions();
      },
      60 * 60 * 1000,
    );
  }

  async register(
    req: RegisterRequest,
    ipAddress?: string,
  ): Promise<AuthResponse> {
    try {
      logger.info("Registering new user:", req.username);

      if (!this.isValidUsername(req.username)) {
        throw new Error(
          "Username must be 3-20 characters and contain only letters, numbers, and underscores",
        );
      }

      if (!this.isValidPassword(req.password)) {
        throw new Error(
          "Password must be at least 8 characters and contain uppercase, lowercase, number, and special character",
        );
      }

      if (req.email && !this.isValidEmail(req.email)) {
        throw new Error("Invalid email address");
      }

      const existingUser = walletDatabase.getUserByUsername(req.username);
      if (existingUser) {
        auditLogger.logSuspiciousActivity(
          undefined,
          ipAddress || "unknown",
          "Attempted registration with existing username",
          { username: req.username },
        );
        throw new Error("Username already exists");
      }

      const userId = this.generateUserId();
      const passwordHash = encryptionService.hashPassword(req.password);
      const now = Date.now();

      walletDatabase.createUser({
        user_id: userId,
        username: req.username,
        password_hash: passwordHash,
        email: req.email,
        created_at: now,
        last_login: now,
        is_locked: 0,
        failed_login_attempts: 0,
        two_fa_enabled: 0,
        password_changed_at: now,
      });

      auditLogger.logUserRegistered(req.username, userId, ipAddress);

      const sessionId = uuidv4();
      const token = this.generateToken(userId, req.username, sessionId);
      const tokenHash = this.hashToken(token);

      walletDatabase.createSession({
        session_id: sessionId,
        user_id: userId,
        token_hash: tokenHash,
        ip_address: ipAddress,
        created_at: now,
        expires_at: now + TOKEN_EXPIRY * 1000,
        last_activity: now,
        is_active: 1,
      });

      logger.info("User registered successfully:", userId);

      return {
        user_id: userId,
        username: req.username,
        token,
        expires_in: TOKEN_EXPIRY,
        session_id: sessionId,
      };
    } catch (error) {
      logger.error("Registration failed:", error);
      throw error;
    }
  }

  async login(req: LoginRequest): Promise<AuthResponse> {
    try {
      logger.info("User login attempt:", req.username);

      const ipAddress = req.ip_address || "unknown";

      const blockedIP = walletDatabase.isIPBlocked(ipAddress);
      if (blockedIP) {
        auditLogger.logSuspiciousActivity(
          undefined,
          ipAddress,
          "Login attempt from blocked IP",
          { username: req.username },
        );
        throw new Error("Access denied. Your IP has been blocked");
      }

      const recentIPAttempts = walletDatabase.getRecentIPAttempts(
        ipAddress,
        15,
      );
      const failedIPAttempts = recentIPAttempts.filter(
        (a) => a.success === 0,
      ).length;

      if (failedIPAttempts >= 20) {
        walletDatabase.blockIP({
          ip_address: ipAddress,
          reason: "Excessive failed login attempts",
          blocked_at: Date.now(),
          blocked_until: Date.now() + 24 * 60 * 60 * 1000,
          block_count: 1,
        });

        auditLogger.logSuspiciousActivity(
          undefined,
          ipAddress,
          "IP blocked due to excessive failed login attempts",
          { username: req.username, failed_attempts: failedIPAttempts },
        );

        throw new Error("Too many failed attempts. Access temporarily blocked");
      }

      const user = walletDatabase.getUserByUsername(req.username);
      if (!user) {
        this.logFailedLogin(
          req.username,
          ipAddress,
          req.user_agent,
          "Invalid credentials",
        );
        throw new Error("Invalid username or password");
      }

      if (
        user.is_locked &&
        user.locked_until &&
        user.locked_until > Date.now()
      ) {
        const remainingMinutes = Math.ceil(
          (user.locked_until - Date.now()) / 60000,
        );
        this.logFailedLogin(
          req.username,
          ipAddress,
          req.user_agent,
          "Account locked",
        );
        throw new Error(
          `Account is locked. Try again in ${remainingMinutes} minutes`,
        );
      }

      if (
        user.is_locked &&
        (!user.locked_until || user.locked_until <= Date.now())
      ) {
        walletDatabase.unlockUser(user.user_id);
      }

      const isValidPassword = encryptionService.verifyPassword(
        req.password,
        user.password_hash,
      );

      if (!isValidPassword) {
        const attempts = walletDatabase.incrementFailedLoginAttempts(
          user.user_id,
        );
        this.logFailedLogin(
          req.username,
          ipAddress,
          req.user_agent,
          "Invalid password",
        );

        if (attempts >= config.maxLoginAttempts) {
          walletDatabase.lockUser(user.user_id, config.accountLockoutDuration);
          auditLogger.logAccountLocked(
            user.user_id,
            req.username,
            "Too many failed login attempts",
          );
          throw new Error("Account locked due to too many failed attempts");
        }

        throw new Error("Invalid username or password");
      }

      if (user.two_fa_enabled && user.two_fa_secret) {
        if (!req.two_fa_token) {
          walletDatabase.logLoginAttempt({
            username: req.username,
            ip_address: ipAddress,
            user_agent: req.user_agent,
            success: 0,
            timestamp: Date.now(),
            failure_reason: "2FA token required",
          });

          return {
            user_id: user.user_id,
            username: user.username,
            token: "",
            expires_in: 0,
            requires_2fa: true,
            session_id: "",
          };
        }

        const is2FAValid = twoFactorService.verifyTwoFactorToken(
          user.two_fa_secret,
          req.two_fa_token,
        );

        if (!is2FAValid) {
          const attempts = walletDatabase.incrementFailedLoginAttempts(
            user.user_id,
          );
          this.logFailedLogin(
            req.username,
            ipAddress,
            req.user_agent,
            "Invalid 2FA token",
          );
          auditLogger.log2FAVerified(user.user_id, user.username, false);

          if (attempts >= config.maxLoginAttempts) {
            walletDatabase.lockUser(
              user.user_id,
              config.accountLockoutDuration,
            );
            auditLogger.logAccountLocked(
              user.user_id,
              req.username,
              "Too many failed 2FA attempts",
            );
            throw new Error(
              "Account locked due to too many failed 2FA attempts",
            );
          }

          throw new Error("Invalid 2FA token");
        }

        auditLogger.log2FAVerified(user.user_id, user.username, true);
      }

      walletDatabase.resetFailedLoginAttempts(user.user_id);
      walletDatabase.updateUserLastLogin(user.user_id);

      walletDatabase.logLoginAttempt({
        username: req.username,
        ip_address: ipAddress,
        user_agent: req.user_agent,
        success: 1,
        timestamp: Date.now(),
      });

      const sessionId = uuidv4();
      const token = this.generateToken(user.user_id, user.username, sessionId);
      const tokenHash = this.hashToken(token);

      walletDatabase.createSession({
        session_id: sessionId,
        user_id: user.user_id,
        token_hash: tokenHash,
        ip_address: ipAddress,
        user_agent: req.user_agent,
        created_at: Date.now(),
        expires_at: Date.now() + TOKEN_EXPIRY * 1000,
        last_activity: Date.now(),
        is_active: 1,
      });

      auditLogger.logLoginSuccess(
        user.username,
        user.user_id,
        ipAddress,
        req.user_agent,
      );
      logger.info("User logged in successfully:", user.user_id);

      return {
        user_id: user.user_id,
        username: user.username,
        token,
        expires_in: TOKEN_EXPIRY,
        session_id: sessionId,
      };
    } catch (error) {
      logger.error("Login failed:", error);
      throw error;
    }
  }

  async logout(sessionId: string): Promise<void> {
    try {
      walletDatabase.invalidateSession(sessionId);
      logger.info("Session invalidated:", sessionId);
    } catch (error) {
      logger.error("Logout failed:", error);
      throw error;
    }
  }

  async logoutAllSessions(userId: string): Promise<void> {
    try {
      walletDatabase.invalidateAllUserSessions(userId);
      logger.info("All sessions invalidated for user:", userId);
    } catch (error) {
      logger.error("Logout all sessions failed:", error);
      throw error;
    }
  }

  verifyToken(token: string): JWTPayload {
    try {
      const decoded = jwt.verify(token, config.jwtSecret) as JWTPayload;

      const tokenHash = this.hashToken(token);
      const session = walletDatabase.getSessionByTokenHash(tokenHash);

      if (!session || session.is_active !== 1) {
        throw new Error("Invalid or expired session");
      }

      if (session.expires_at < Date.now()) {
        walletDatabase.invalidateSession(session.session_id);
        throw new Error("Session expired");
      }

      walletDatabase.updateSessionActivity(session.session_id);

      return decoded;
    } catch (error) {
      throw new Error("Invalid or expired token");
    }
  }

  private generateToken(
    userId: string,
    username: string,
    sessionId: string,
  ): string {
    const payload = {
      user_id: userId,
      username,
      session_id: sessionId,
    };

    return jwt.sign(payload, config.jwtSecret, {
      expiresIn: TOKEN_EXPIRY,
    });
  }

  private hashToken(token: string): string {
    return crypto.createHash("sha256").update(token).digest("hex");
  }

  private generateUserId(): string {
    return `user_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  private logFailedLogin(
    username: string,
    ipAddress: string,
    userAgent?: string,
    reason?: string,
  ): void {
    walletDatabase.logLoginAttempt({
      username,
      ip_address: ipAddress,
      user_agent: userAgent,
      success: 0,
      timestamp: Date.now(),
      failure_reason: reason,
    });

    auditLogger.logLoginFailed(username, ipAddress, reason);
  }

  private isValidUsername(username: string): boolean {
    return /^[a-zA-Z0-9_]{3,20}$/.test(username);
  }

  private isValidPassword(password: string): boolean {
    if (password.length < 8) return false;

    const hasUpperCase = /[A-Z]/.test(password);
    const hasLowerCase = /[a-z]/.test(password);
    const hasNumbers = /\d/.test(password);
    const hasSpecialChar = /[!@#$%^&*(),.?":{}|<>]/.test(password);

    return hasUpperCase && hasLowerCase && hasNumbers && hasSpecialChar;
  }

  private isValidEmail(email: string): boolean {
    return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
  }

  getUserSessions(userId: string): any[] {
    const sessions = walletDatabase.getUserSessions(userId);
    return sessions.map((s) => ({
      session_id: s.session_id,
      ip_address: s.ip_address,
      user_agent: s.user_agent,
      created_at: s.created_at,
      last_activity: s.last_activity,
      expires_at: s.expires_at,
    }));
  }
}

export const authService = new AuthService();
