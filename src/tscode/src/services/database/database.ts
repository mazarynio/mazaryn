import Database from "better-sqlite3";
import { logger } from "../../core/logger.js";
import path from "path";
import fs from "fs";

const DB_DIR = path.join(process.cwd(), "data");
const DB_PATH = path.join(DB_DIR, "wallets.db");

if (!fs.existsSync(DB_DIR)) {
  fs.mkdirSync(DB_DIR, { recursive: true });
}

const db = new Database(DB_PATH);

db.exec(`
  CREATE TABLE IF NOT EXISTS wallets (
    wallet_id TEXT PRIMARY KEY,
    user_id TEXT NOT NULL,
    public_key TEXT NOT NULL,
    private_key TEXT NOT NULL,
    wallet_name TEXT,
    created_at INTEGER NOT NULL
  );

  CREATE INDEX IF NOT EXISTS idx_user_id ON wallets(user_id);
  CREATE INDEX IF NOT EXISTS idx_public_key ON wallets(public_key);
`);

logger.info("Database initialized at:", DB_PATH);

export interface WalletRecord {
  wallet_id: string;
  user_id: string;
  public_key: string;
  private_key: string;
  wallet_name?: string;
  created_at: number;
}

export class WalletDatabase {
  saveWallet(wallet: WalletRecord): void {
    const stmt = db.prepare(`
      INSERT INTO wallets (wallet_id, user_id, public_key, private_key, wallet_name, created_at)
      VALUES (?, ?, ?, ?, ?, ?)
    `);
    stmt.run(
      wallet.wallet_id,
      wallet.user_id,
      wallet.public_key,
      wallet.private_key,
      wallet.wallet_name || null,
      wallet.created_at,
    );
  }

  getWallet(walletId: string): WalletRecord | null {
    const stmt = db.prepare("SELECT * FROM wallets WHERE wallet_id = ?");
    return stmt.get(walletId) as WalletRecord | null;
  }

  getUserWallets(userId: string): WalletRecord[] {
    const stmt = db.prepare(
      "SELECT * FROM wallets WHERE user_id = ? ORDER BY created_at DESC",
    );
    return stmt.all(userId) as WalletRecord[];
  }

  deleteWallet(walletId: string, userId: string): boolean {
    const stmt = db.prepare(
      "DELETE FROM wallets WHERE wallet_id = ? AND user_id = ?",
    );
    const result = stmt.run(walletId, userId);
    return result.changes > 0;
  }

  getAllWallets(): WalletRecord[] {
    const stmt = db.prepare("SELECT * FROM wallets ORDER BY created_at DESC");
    return stmt.all() as WalletRecord[];
  }
}

export const walletDatabase = new WalletDatabase();
