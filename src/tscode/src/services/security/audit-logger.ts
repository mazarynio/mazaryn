import fs from "fs";
import path from "path";
import { config } from "../../config/index.js";

export enum AuditEventType {
  USER_REGISTERED = "USER_REGISTERED",
  USER_LOGIN_SUCCESS = "USER_LOGIN_SUCCESS",
  USER_LOGIN_FAILED = "USER_LOGIN_FAILED",
  USER_LOGOUT = "USER_LOGOUT",
  WALLET_CREATED = "WALLET_CREATED",
  WALLET_IMPORTED = "WALLET_IMPORTED",
  WALLET_DELETED = "WALLET_DELETED",
  PRIVATE_KEY_EXPORTED = "PRIVATE_KEY_EXPORTED",
  TRANSFER_INITIATED = "TRANSFER_INITIATED",
  TRANSFER_COMPLETED = "TRANSFER_COMPLETED",
  TRANSFER_FAILED = "TRANSFER_FAILED",
  TOKEN_TRANSFER = "TOKEN_TRANSFER",
  NFT_TRANSFER = "NFT_TRANSFER",
  ACCOUNT_LOCKED = "ACCOUNT_LOCKED",
  ACCOUNT_UNLOCKED = "ACCOUNT_UNLOCKED",
  TWO_FA_ENABLED = "TWO_FA_ENABLED",
  TWO_FA_DISABLED = "TWO_FA_DISABLED",
  TWO_FA_VERIFIED = "TWO_FA_VERIFIED",
  SUSPICIOUS_ACTIVITY = "SUSPICIOUS_ACTIVITY",
  RATE_LIMIT_EXCEEDED = "RATE_LIMIT_EXCEEDED",
  UNAUTHORIZED_ACCESS = "UNAUTHORIZED_ACCESS",
}

export interface AuditLogEntry {
  timestamp: number;
  event_type: AuditEventType;
  user_id?: string;
  username?: string;
  ip_address?: string;
  user_agent?: string;
  details?: any;
  success: boolean;
  error_message?: string;
}

export class AuditLogger {
  private logPath: string;

  constructor() {
    this.logPath = config.auditLogPath;
    this.ensureLogDirectory();
  }

  private ensureLogDirectory(): void {
    const dir = path.dirname(this.logPath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
  }

  log(entry: AuditLogEntry): void {
    if (!config.auditLogEnabled) {
      return;
    }

    const logEntry = {
      ...entry,
      timestamp: entry.timestamp || Date.now(),
    };

    const logLine = JSON.stringify(logEntry) + "\n";

    try {
      fs.appendFileSync(this.logPath, logLine, "utf8");
    } catch (error) {
      console.error("Failed to write audit log:", error);
    }
  }

  logUserRegistered(
    username: string,
    userId: string,
    ipAddress?: string,
  ): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.USER_REGISTERED,
      user_id: userId,
      username,
      ip_address: ipAddress,
      success: true,
    });
  }

  logLoginSuccess(
    username: string,
    userId: string,
    ipAddress?: string,
    userAgent?: string,
  ): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.USER_LOGIN_SUCCESS,
      user_id: userId,
      username,
      ip_address: ipAddress,
      user_agent: userAgent,
      success: true,
    });
  }

  logLoginFailed(username: string, ipAddress?: string, reason?: string): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.USER_LOGIN_FAILED,
      username,
      ip_address: ipAddress,
      success: false,
      error_message: reason,
    });
  }

  logWalletCreated(userId: string, walletId: string, publicKey: string): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.WALLET_CREATED,
      user_id: userId,
      success: true,
      details: { wallet_id: walletId, public_key: publicKey },
    });
  }

  logPrivateKeyExported(
    userId: string,
    walletId: string,
    ipAddress?: string,
  ): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.PRIVATE_KEY_EXPORTED,
      user_id: userId,
      ip_address: ipAddress,
      success: true,
      details: { wallet_id: walletId },
    });
  }

  logTransfer(
    userId: string,
    fromWallet: string,
    toAddress: string,
    amount: number,
    success: boolean,
    signature?: string,
    error?: string,
  ): void {
    this.log({
      timestamp: Date.now(),
      event_type: success
        ? AuditEventType.TRANSFER_COMPLETED
        : AuditEventType.TRANSFER_FAILED,
      user_id: userId,
      success,
      details: {
        from_wallet: fromWallet,
        to_address: toAddress,
        amount,
        signature,
      },
      error_message: error,
    });
  }

  logAccountLocked(userId: string, username: string, reason: string): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.ACCOUNT_LOCKED,
      user_id: userId,
      username,
      success: true,
      details: { reason },
    });
  }

  logSuspiciousActivity(
    userId: string | undefined,
    ipAddress: string,
    activity: string,
    details?: any,
  ): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.SUSPICIOUS_ACTIVITY,
      user_id: userId,
      ip_address: ipAddress,
      success: false,
      details: { activity, ...details },
    });
  }

  logUnauthorizedAccess(
    userId: string | undefined,
    ipAddress: string,
    resource: string,
  ): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.UNAUTHORIZED_ACCESS,
      user_id: userId,
      ip_address: ipAddress,
      success: false,
      details: { resource },
    });
  }

  logRateLimitExceeded(ipAddress: string, endpoint: string): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.RATE_LIMIT_EXCEEDED,
      ip_address: ipAddress,
      success: false,
      details: { endpoint },
    });
  }

  log2FAEnabled(userId: string, username: string): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.TWO_FA_ENABLED,
      user_id: userId,
      username,
      success: true,
    });
  }

  log2FAVerified(userId: string, username: string, success: boolean): void {
    this.log({
      timestamp: Date.now(),
      event_type: AuditEventType.TWO_FA_VERIFIED,
      user_id: userId,
      username,
      success,
    });
  }

  queryLogs(filters: {
    startTime?: number;
    endTime?: number;
    userId?: string;
    eventType?: AuditEventType;
    ipAddress?: string;
  }): AuditLogEntry[] {
    if (!config.auditLogEnabled) {
      return [];
    }

    try {
      const content = fs.readFileSync(this.logPath, "utf8");
      const lines = content
        .trim()
        .split("\n")
        .filter((line) => line.length > 0);

      let entries = lines
        .map((line) => {
          try {
            return JSON.parse(line) as AuditLogEntry;
          } catch {
            return null;
          }
        })
        .filter((entry) => entry !== null) as AuditLogEntry[];

      if (filters.startTime) {
        entries = entries.filter((e) => e.timestamp >= filters.startTime!);
      }

      if (filters.endTime) {
        entries = entries.filter((e) => e.timestamp <= filters.endTime!);
      }

      if (filters.userId) {
        entries = entries.filter((e) => e.user_id === filters.userId);
      }

      if (filters.eventType) {
        entries = entries.filter((e) => e.event_type === filters.eventType);
      }

      if (filters.ipAddress) {
        entries = entries.filter((e) => e.ip_address === filters.ipAddress);
      }

      return entries;
    } catch (error) {
      console.error("Failed to read audit log:", error);
      return [];
    }
  }
}

export const auditLogger = new AuditLogger();
