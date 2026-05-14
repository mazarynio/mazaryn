import Database from "better-sqlite3";
import path from "path";
import fs from "fs";
import { logger } from "../../core/logger.js";
import type { SuiNetwork, SuiTxType } from "../../core/sui-types.js";

const DB_PATH = process.env.SUI_DB_PATH || "./data/sui-wallets.db";

export interface SuiWalletRecord {
  wallet_id: string;
  user_id: string;
  sui_address: string;
  public_key: string;
  encrypted_private_key: string;
  encryption_iv: string;
  encryption_tag: string;
  label?: string;
  scheme: string;
  network: SuiNetwork;
  is_primary: number;
  created_at: number;
  last_used?: number;
}

export interface SuiTransactionRecord {
  tx_id: string;
  wallet_id: string;
  user_id: string;
  digest: string;
  tx_type: SuiTxType;
  sender_address: string;
  recipient_address?: string;
  amount_mist?: string;
  coin_type?: string;
  object_id?: string;
  package_id?: string;
  module_name?: string;
  function_name?: string;
  validator_address?: string;
  staked_sui_id?: string;
  gas_computation: string;
  gas_storage: string;
  gas_storage_rebate: string;
  status: string;
  effects_status: string;
  checkpoint?: string;
  epoch?: string;
  error_message?: string;
  network: SuiNetwork;
  created_at: number;
}

export class SuiDatabase {
  private db: Database.Database;

  constructor() {
    const dbDir = path.dirname(DB_PATH);
    if (!fs.existsSync(dbDir)) {
      fs.mkdirSync(dbDir, { recursive: true });
    }

    this.db = new Database(DB_PATH);
    this.db.pragma("journal_mode = WAL");
    this.db.pragma("foreign_keys = ON");
    this.initializeDatabase();
    this.runMigrations();
    logger.info("Sui Database initialized at:", DB_PATH);
  }

  private initializeDatabase(): void {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS sui_wallets (
        wallet_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        sui_address TEXT NOT NULL UNIQUE,
        public_key TEXT NOT NULL,
        encrypted_private_key TEXT NOT NULL,
        encryption_iv TEXT NOT NULL,
        encryption_tag TEXT NOT NULL,
        label TEXT,
        scheme TEXT NOT NULL DEFAULT 'ed25519',
        network TEXT NOT NULL DEFAULT 'mainnet',
        is_primary INTEGER NOT NULL DEFAULT 0,
        created_at INTEGER NOT NULL,
        last_used INTEGER
      );

      CREATE INDEX IF NOT EXISTS idx_sui_wallet_user ON sui_wallets(user_id);
      CREATE INDEX IF NOT EXISTS idx_sui_wallet_address ON sui_wallets(sui_address);
      CREATE INDEX IF NOT EXISTS idx_sui_wallet_network ON sui_wallets(network);
      CREATE INDEX IF NOT EXISTS idx_sui_wallet_primary ON sui_wallets(user_id, is_primary);

      CREATE TABLE IF NOT EXISTS sui_transactions (
        tx_id TEXT PRIMARY KEY,
        wallet_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        digest TEXT NOT NULL UNIQUE,
        tx_type TEXT NOT NULL,
        sender_address TEXT NOT NULL,
        recipient_address TEXT,
        amount_mist TEXT,
        coin_type TEXT,
        object_id TEXT,
        package_id TEXT,
        module_name TEXT,
        function_name TEXT,
        validator_address TEXT,
        staked_sui_id TEXT,
        gas_computation TEXT NOT NULL DEFAULT '0',
        gas_storage TEXT NOT NULL DEFAULT '0',
        gas_storage_rebate TEXT NOT NULL DEFAULT '0',
        status TEXT NOT NULL DEFAULT 'pending',
        effects_status TEXT NOT NULL DEFAULT '',
        checkpoint TEXT,
        epoch TEXT,
        error_message TEXT,
        network TEXT NOT NULL DEFAULT 'mainnet',
        created_at INTEGER NOT NULL
      );

      CREATE INDEX IF NOT EXISTS idx_sui_tx_wallet ON sui_transactions(wallet_id);
      CREATE INDEX IF NOT EXISTS idx_sui_tx_user ON sui_transactions(user_id);
      CREATE INDEX IF NOT EXISTS idx_sui_tx_digest ON sui_transactions(digest);
      CREATE INDEX IF NOT EXISTS idx_sui_tx_type ON sui_transactions(tx_type);
      CREATE INDEX IF NOT EXISTS idx_sui_tx_sender ON sui_transactions(sender_address);
      CREATE INDEX IF NOT EXISTS idx_sui_tx_created ON sui_transactions(created_at DESC);
      CREATE INDEX IF NOT EXISTS idx_sui_tx_network ON sui_transactions(network);

      CREATE TABLE IF NOT EXISTS sui_schema_migrations (
        id INTEGER PRIMARY KEY,
        applied_at INTEGER NOT NULL
      );
    `);
  }

  private runMigrations(): void {
    const migrations: { id: number; sql: string }[] = [];

    for (const migration of migrations) {
      const already = this.db
        .prepare("SELECT id FROM sui_schema_migrations WHERE id = ?")
        .get(migration.id);

      if (!already) {
        try {
          this.db.exec(migration.sql);
          logger.info(`Sui migration ${migration.id} applied`);
        } catch {
          logger.info(`Sui migration ${migration.id} skipped (may already exist)`);
        }

        this.db
          .prepare("INSERT INTO sui_schema_migrations (id, applied_at) VALUES (?, ?)")
          .run(migration.id, Date.now());
      }
    }
  }

  createWallet(wallet: Omit<SuiWalletRecord, "is_primary"> & { is_primary?: number }): void {
    const existingWallets = this.getUserWallets(wallet.user_id);
    const isFirst = existingWallets.length === 0;

    this.db
      .prepare(
        `INSERT INTO sui_wallets (
          wallet_id, user_id, sui_address, public_key,
          encrypted_private_key, encryption_iv, encryption_tag,
          label, scheme, network, is_primary, created_at
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
      )
      .run(
        wallet.wallet_id,
        wallet.user_id,
        wallet.sui_address,
        wallet.public_key,
        wallet.encrypted_private_key,
        wallet.encryption_iv,
        wallet.encryption_tag,
        wallet.label ?? null,
        wallet.scheme,
        wallet.network,
        isFirst ? 1 : (wallet.is_primary ?? 0),
        wallet.created_at,
      );

    logger.info(`Sui wallet created: ${wallet.wallet_id} for user ${wallet.user_id}`);
  }

  getWallet(walletId: string): SuiWalletRecord | null {
    return (
      (this.db
        .prepare("SELECT * FROM sui_wallets WHERE wallet_id = ?")
        .get(walletId) as SuiWalletRecord) ?? null
    );
  }

  getWalletByAddress(suiAddress: string): SuiWalletRecord | null {
    return (
      (this.db
        .prepare("SELECT * FROM sui_wallets WHERE sui_address = ?")
        .get(suiAddress) as SuiWalletRecord) ?? null
    );
  }

  getUserWallets(userId: string): SuiWalletRecord[] {
    return this.db
      .prepare("SELECT * FROM sui_wallets WHERE user_id = ? ORDER BY created_at ASC")
      .all(userId) as SuiWalletRecord[];
  }

  getUserWalletsByNetwork(userId: string, network: SuiNetwork): SuiWalletRecord[] {
    return this.db
      .prepare("SELECT * FROM sui_wallets WHERE user_id = ? AND network = ? ORDER BY created_at ASC")
      .all(userId, network) as SuiWalletRecord[];
  }

  getPrimaryWallet(userId: string): SuiWalletRecord | null {
    return (
      (this.db
        .prepare("SELECT * FROM sui_wallets WHERE user_id = ? AND is_primary = 1")
        .get(userId) as SuiWalletRecord) ?? null
    );
  }

  setPrimaryWallet(userId: string, walletId: string): void {
    const setPrimary = this.db.transaction(() => {
      this.db
        .prepare("UPDATE sui_wallets SET is_primary = 0 WHERE user_id = ?")
        .run(userId);
      this.db
        .prepare("UPDATE sui_wallets SET is_primary = 1 WHERE wallet_id = ? AND user_id = ?")
        .run(walletId, userId);
    });
    setPrimary();
    logger.info(`Set primary Sui wallet: ${walletId} for user ${userId}`);
  }

  updateWalletLabel(walletId: string, label: string): void {
    this.db
      .prepare("UPDATE sui_wallets SET label = ? WHERE wallet_id = ?")
      .run(label, walletId);
  }

  updateWalletLastUsed(walletId: string): void {
    this.db
      .prepare("UPDATE sui_wallets SET last_used = ? WHERE wallet_id = ?")
      .run(Date.now(), walletId);
  }

  deleteWallet(walletId: string): void {
    const wallet = this.getWallet(walletId);
    if (!wallet) return;

    const wasPrimary = wallet.is_primary === 1;
    const userId = wallet.user_id;

    this.db.prepare("DELETE FROM sui_wallets WHERE wallet_id = ?").run(walletId);

    if (wasPrimary) {
      const remaining = this.getUserWallets(userId);
      if (remaining.length > 0) {
        this.db
          .prepare("UPDATE sui_wallets SET is_primary = 1 WHERE wallet_id = ?")
          .run(remaining[0].wallet_id);
        logger.info(`Promoted ${remaining[0].wallet_id} to primary after deletion`);
      }
    }

    logger.info(`Sui wallet deleted: ${walletId}`);
  }

  walletExists(walletId: string): boolean {
    const row = this.db
      .prepare("SELECT wallet_id FROM sui_wallets WHERE wallet_id = ?")
      .get(walletId);
    return row !== undefined;
  }

  addressExists(suiAddress: string): boolean {
    const row = this.db
      .prepare("SELECT wallet_id FROM sui_wallets WHERE sui_address = ?")
      .get(suiAddress);
    return row !== undefined;
  }

  getAllWallets(): SuiWalletRecord[] {
    return this.db
      .prepare("SELECT * FROM sui_wallets ORDER BY created_at ASC")
      .all() as SuiWalletRecord[];
  }

  createTransaction(record: SuiTransactionRecord): void {
    this.db
      .prepare(
        `INSERT OR IGNORE INTO sui_transactions (
          tx_id, wallet_id, user_id, digest, tx_type, sender_address,
          recipient_address, amount_mist, coin_type, object_id, package_id,
          module_name, function_name, validator_address, staked_sui_id,
          gas_computation, gas_storage, gas_storage_rebate,
          status, effects_status, checkpoint, epoch, error_message,
          network, created_at
        ) VALUES (
          ?, ?, ?, ?, ?, ?,
          ?, ?, ?, ?, ?,
          ?, ?, ?, ?,
          ?, ?, ?,
          ?, ?, ?, ?, ?,
          ?, ?
        )`,
      )
      .run(
        record.tx_id,
        record.wallet_id,
        record.user_id,
        record.digest,
        record.tx_type,
        record.sender_address,
        record.recipient_address ?? null,
        record.amount_mist ?? null,
        record.coin_type ?? null,
        record.object_id ?? null,
        record.package_id ?? null,
        record.module_name ?? null,
        record.function_name ?? null,
        record.validator_address ?? null,
        record.staked_sui_id ?? null,
        record.gas_computation,
        record.gas_storage,
        record.gas_storage_rebate,
        record.status,
        record.effects_status,
        record.checkpoint ?? null,
        record.epoch ?? null,
        record.error_message ?? null,
        record.network,
        record.created_at,
      );
  }

  getTransaction(txId: string): SuiTransactionRecord | null {
    return (
      (this.db
        .prepare("SELECT * FROM sui_transactions WHERE tx_id = ?")
        .get(txId) as SuiTransactionRecord) ?? null
    );
  }

  getTransactionByDigest(digest: string): SuiTransactionRecord | null {
    return (
      (this.db
        .prepare("SELECT * FROM sui_transactions WHERE digest = ?")
        .get(digest) as SuiTransactionRecord) ?? null
    );
  }

  getWalletTransactions(walletId: string, limit = 50, offset = 0): SuiTransactionRecord[] {
    return this.db
      .prepare(
        "SELECT * FROM sui_transactions WHERE wallet_id = ? ORDER BY created_at DESC LIMIT ? OFFSET ?",
      )
      .all(walletId, limit, offset) as SuiTransactionRecord[];
  }

  getWalletTransactionCount(walletId: string): number {
    const row = this.db
      .prepare("SELECT COUNT(*) as count FROM sui_transactions WHERE wallet_id = ?")
      .get(walletId) as { count: number };
    return row.count;
  }

  getUserTransactions(userId: string, limit = 50, offset = 0): SuiTransactionRecord[] {
    return this.db
      .prepare(
        "SELECT * FROM sui_transactions WHERE user_id = ? ORDER BY created_at DESC LIMIT ? OFFSET ?",
      )
      .all(userId, limit, offset) as SuiTransactionRecord[];
  }

  getTransactionsByType(walletId: string, txType: SuiTxType, limit = 50): SuiTransactionRecord[] {
    return this.db
      .prepare(
        "SELECT * FROM sui_transactions WHERE wallet_id = ? AND tx_type = ? ORDER BY created_at DESC LIMIT ?",
      )
      .all(walletId, txType, limit) as SuiTransactionRecord[];
  }

  updateTransactionStatus(digest: string, status: string, effectsStatus: string, checkpoint?: string, errorMessage?: string): void {
    this.db
      .prepare(
        `UPDATE sui_transactions
         SET status = ?, effects_status = ?, checkpoint = ?, error_message = ?
         WHERE digest = ?`,
      )
      .run(status, effectsStatus, checkpoint ?? null, errorMessage ?? null, digest);
  }

  close(): void {
    this.db.close();
  }
}

export const suiDatabase = new SuiDatabase();
