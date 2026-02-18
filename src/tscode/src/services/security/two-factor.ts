import speakeasy from "speakeasy";
import QRCode from "qrcode";
import { walletDatabase } from "../database/database.js";
import { logger } from "../../core/logger.js";
import { auditLogger } from "./audit-logger.js";

export interface TwoFactorSetupResponse {
  secret: string;
  qr_code: string;
  manual_entry_key: string;
}

export class TwoFactorService {
  async setupTwoFactor(
    userId: string,
    username: string,
  ): Promise<TwoFactorSetupResponse> {
    try {
      const secret = speakeasy.generateSecret({
        name: `Solana Wallet (${username})`,
        issuer: "Solana Wallet Service",
        length: 32,
      });

      const qrCodeUrl = await QRCode.toDataURL(secret.otpauth_url || "");

      return {
        secret: secret.base32,
        qr_code: qrCodeUrl,
        manual_entry_key: secret.base32,
      };
    } catch (error) {
      logger.error("Failed to setup 2FA:", error);
      throw error;
    }
  }

  async enableTwoFactor(
    userId: string,
    username: string,
    secret: string,
    token: string,
  ): Promise<boolean> {
    try {
      const verified = speakeasy.totp.verify({
        secret,
        encoding: "base32",
        token,
        window: 2,
      });

      if (!verified) {
        return false;
      }

      walletDatabase.updateUser(userId, {
        two_fa_enabled: 1,
        two_fa_secret: secret,
      });

      auditLogger.log2FAEnabled(userId, username);
      logger.info("2FA enabled for user:", userId);

      return true;
    } catch (error) {
      logger.error("Failed to enable 2FA:", error);
      throw error;
    }
  }

  async disableTwoFactor(
    userId: string,
    username: string,
    token: string,
  ): Promise<boolean> {
    try {
      const user = walletDatabase.getUser(userId);
      if (!user || !user.two_fa_secret) {
        return false;
      }

      const verified = speakeasy.totp.verify({
        secret: user.two_fa_secret,
        encoding: "base32",
        token,
        window: 2,
      });

      if (!verified) {
        return false;
      }

      walletDatabase.updateUser(userId, {
        two_fa_enabled: 0,
        two_fa_secret: null,
      });

      auditLogger.log({
        timestamp: Date.now(),
        event_type: auditLogger.constructor.name.includes("AuditLogger")
          ? ("TWO_FA_DISABLED" as any)
          : ("TWO_FA_DISABLED" as any),
        user_id: userId,
        username,
        success: true,
      });

      logger.info("2FA disabled for user:", userId);

      return true;
    } catch (error) {
      logger.error("Failed to disable 2FA:", error);
      throw error;
    }
  }

  verifyTwoFactorToken(secret: string, token: string): boolean {
    try {
      return speakeasy.totp.verify({
        secret,
        encoding: "base32",
        token,
        window: 2,
      });
    } catch (error) {
      logger.error("Failed to verify 2FA token:", error);
      return false;
    }
  }

  generateBackupCodes(count: number = 10): string[] {
    const codes: string[] = [];
    for (let i = 0; i < count; i++) {
      const code = Math.random().toString(36).substring(2, 10).toUpperCase();
      codes.push(code);
    }
    return codes;
  }
}

export const twoFactorService = new TwoFactorService();
