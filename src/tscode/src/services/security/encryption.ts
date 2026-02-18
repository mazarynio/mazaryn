import crypto from "crypto";
import { config } from "../../config/index.js";

const ALGORITHM = "aes-256-gcm";
const KEY_LENGTH = 32;
const IV_LENGTH = 16;
const AUTH_TAG_LENGTH = 16;
const SALT_LENGTH = 64;

export class EncryptionService {
  private key: Buffer;

  constructor() {
    this.key = this.deriveKey(config.encryptionKey);
  }

  private deriveKey(secret: string): Buffer {
    return crypto.scryptSync(secret, "solana-wallet-salt", KEY_LENGTH);
  }

  encrypt(plaintext: string): string {
    const iv = crypto.randomBytes(IV_LENGTH);
    const cipher = crypto.createCipheriv(ALGORITHM, this.key, iv);

    let encrypted = cipher.update(plaintext, "utf8", "hex");
    encrypted += cipher.final("hex");

    const authTag = cipher.getAuthTag();

    const result = Buffer.concat([iv, authTag, Buffer.from(encrypted, "hex")]);

    return result.toString("base64");
  }

  decrypt(ciphertext: string): string {
    const buffer = Buffer.from(ciphertext, "base64");

    const iv = buffer.slice(0, IV_LENGTH);
    const authTag = buffer.slice(IV_LENGTH, IV_LENGTH + AUTH_TAG_LENGTH);
    const encrypted = buffer.slice(IV_LENGTH + AUTH_TAG_LENGTH);

    const decipher = crypto.createDecipheriv(ALGORITHM, this.key, iv);
    decipher.setAuthTag(authTag);

    let decrypted = decipher.update(encrypted.toString("hex"), "hex", "utf8");
    decrypted += decipher.final("utf8");

    return decrypted;
  }

  hashPassword(password: string): string {
    const salt = crypto.randomBytes(SALT_LENGTH);
    const hash = crypto.pbkdf2Sync(password, salt, 100000, 64, "sha512");

    return salt.toString("hex") + ":" + hash.toString("hex");
  }

  verifyPassword(password: string, hashedPassword: string): boolean {
    const [saltHex, hashHex] = hashedPassword.split(":");

    if (!saltHex || !hashHex) {
      return false;
    }

    const salt = Buffer.from(saltHex, "hex");
    const hash = Buffer.from(hashHex, "hex");
    const testHash = crypto.pbkdf2Sync(password, salt, 100000, 64, "sha512");

    return crypto.timingSafeEqual(hash, testHash);
  }
}

export const encryptionService = new EncryptionService();
