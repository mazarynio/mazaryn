import crypto from "crypto";
import { config } from "../../config/index.js";

const ALGORITHM = "aes-256-gcm";
const IV_LENGTH = 16;
const TAG_LENGTH = 16;
const KEY_LENGTH = 32;

export class EncryptionService {
  private key: Buffer;

  constructor() {
    const rawKey = config.encryptionKey;
    this.key = crypto.scryptSync(rawKey, "aptos-salt", KEY_LENGTH);
  }

  encrypt(data: string): { encryptedData: string; iv: string; tag: string } {
    const iv = crypto.randomBytes(IV_LENGTH);
    const cipher = crypto.createCipheriv(
      ALGORITHM,
      this.key,
      iv,
    ) as crypto.CipherGCM;

    const encrypted = Buffer.concat([
      cipher.update(data, "utf8"),
      cipher.final(),
    ]);

    const tag = cipher.getAuthTag();

    return {
      encryptedData: encrypted.toString("hex"),
      iv: iv.toString("hex"),
      tag: tag.toString("hex"),
    };
  }

  decrypt(encryptedData: string, iv: string, tag: string): string {
    const decipher = crypto.createDecipheriv(
      ALGORITHM,
      this.key,
      Buffer.from(iv, "hex"),
    ) as crypto.DecipherGCM;

    decipher.setAuthTag(Buffer.from(tag, "hex"));

    const decrypted = Buffer.concat([
      decipher.update(Buffer.from(encryptedData, "hex")),
      decipher.final(),
    ]);

    return decrypted.toString("utf8");
  }
}

export const encryptionService = new EncryptionService();
