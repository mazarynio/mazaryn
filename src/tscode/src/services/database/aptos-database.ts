import Database from "better-sqlite3";
import path from "path";
import fs from "fs";
import { logger } from "../../core/logger.js";
import type { AptosWalletRecord } from "../../core/aptos-types.js";

const DB_PATH = process.env.APTOS_DB_PATH || "./data/aptos-wallets.db";

export interface AptosTransactionRecord {
  tx_id: string;
  wallet_id: string;
  user_id: string;
  from_address: string;
  to_address: string;
  transaction_hash: string;
  tx_type: string;
  amount_octas: number;
  coin_type?: string;
  metadata_address?: string;
  pool_address?: string;
  collection_name?: string;
  token_address?: string;
  gas_used: number;
  gas_unit_price: number;
  version: string;
  vm_status: string;
  success: boolean;
  network: string;
  created_at: number;
}

export class AptosDatabase {
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
    logger.info("Aptos Database initialized at:", DB_PATH);
  }

  private initializeDatabase(): void {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS aptos_wallets (
        wallet_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        account_address TEXT NOT NULL UNIQUE,
        public_key TEXT NOT NULL,
        encrypted_private_key TEXT NOT NULL,
        encryption_iv TEXT NOT NULL,
        encryption_tag TEXT NOT NULL,
        label TEXT,
        signing_scheme TEXT NOT NULL DEFAULT 'ed25519',
        network TEXT NOT NULL DEFAULT 'testnet',
        created_at INTEGER NOT NULL,
        last_used INTEGER,
        is_primary INTEGER NOT NULL DEFAULT 0
      );

      CREATE INDEX IF NOT EXISTS idx_aptos_wallet_user ON aptos_wallets(user_id);
      CREATE INDEX IF NOT EXISTS idx_aptos_wallet_address ON aptos_wallets(account_address);
      CREATE INDEX IF NOT EXISTS idx_aptos_wallet_network ON aptos_wallets(network);

      CREATE TABLE IF NOT EXISTS aptos_transactions (
        tx_id TEXT PRIMARY KEY,
        wallet_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        from_address TEXT NOT NULL,
        to_address TEXT NOT NULL,
        transaction_hash TEXT NOT NULL UNIQUE,
        tx_type TEXT NOT NULL,
        amount_octas INTEGER NOT NULL DEFAULT 0,
        coin_type TEXT,
        metadata_address TEXT,
        pool_address TEXT,
        collection_name TEXT,
        token_address TEXT,
        gas_used INTEGER NOT NULL DEFAULT 0,
        gas_unit_price INTEGER NOT NULL DEFAULT 0,
        version TEXT NOT NULL DEFAULT '',
        vm_status TEXT NOT NULL DEFAULT '',
        success INTEGER NOT NULL DEFAULT 1,
        network TEXT NOT NULL DEFAULT 'testnet',
        created_at INTEGER NOT NULL
      );

      CREATE INDEX IF NOT EXISTS idx_aptos_tx_wallet ON aptos_transactions(wallet_id);
      CREATE INDEX IF NOT EXISTS idx_aptos_tx_user ON aptos_transactions(user_id);
      CREATE INDEX IF NOT EXISTS idx_aptos_tx_hash ON aptos_transactions(transaction_hash);
      CREATE INDEX IF NOT EXISTS idx_aptos_tx_type ON aptos_transactions(tx_type);
      CREATE INDEX IF NOT EXISTS idx_aptos_tx_created ON aptos_transactions(created_at);

      CREATE TABLE IF NOT EXISTS aptos_schema_migrations (
        id INTEGER PRIMARY KEY,
        applied_at INTEGER NOT NULL
      );
    `);
  }

  private runMigrations(): void {
    const migrations: { id: number; sql: string }[] = [];

    for (const migration of migrations) {
      const already = this.db
        .prepare("SELECT id FROM aptos_schema_migrations WHERE id = ?")
        .get(migration.id);

      if (!already) {
        try {
          this.db.exec(migration.sql);
          logger.info(`Aptos Migration ${migration.id} applied`);
        } catch {
          logger.info(
            `Aptos Migration ${migration.id} skipped (column may already exist)`,
          );
        }

        this.db
          .prepare(
            "INSERT INTO aptos_schema_migrations (id, applied_at) VALUES (?, ?)",
          )
          .run(migration.id, Date.now());
      }
    }
  }

  createWallet(wallet: {
    wallet_id: string;
    user_id: string;
    account_address: string;
    public_key: string;
    encrypted_private_key: string;
    encryption_iv: string;
    encryption_tag: string;
    label?: string;
    signing_scheme: string;
    network: string;
    created_at: number;
    is_primary?: boolean;
  }): void {
    const existingWallets = this.getUserWallets(wallet.user_id);
    const isFirst = existingWallets.length === 0;

    const stmt = this.db.prepare(`
      INSERT INTO aptos_wallets (
        wallet_id, user_id, account_address, public_key,
        encrypted_private_key, encryption_iv, encryption_tag,
        label, signing_scheme, network, created_at, is_primary
      ) VALUES (
        @wallet_id, @user_id, @account_address, @public_key,
        @encrypted_private_key, @encryption_iv, @encryption_tag,
        @label, @signing_scheme, @network, @created_at, @is_primary
      )
    `);

    stmt.run({
      ...wallet,
      label: wallet.label ?? null,
      is_primary: isFirst || (wallet.is_primary ?? false) ? 1 : 0,
    });
  }

  getWallet(walletId: string): AptosWalletRecord | undefined {
    const row = this.db
      .prepare("SELECT * FROM aptos_wallets WHERE wallet_id = ?")
      .get(walletId) as any;
    return row ? this.rowToRecord(row) : undefined;
  }

  getWalletByAddress(accountAddress: string): AptosWalletRecord | undefined {
    const row = this.db
      .prepare("SELECT * FROM aptos_wallets WHERE account_address = ?")
      .get(accountAddress) as any;
    return row ? this.rowToRecord(row) : undefined;
  }

  getUserWallets(userId: string): AptosWalletRecord[] {
    const rows = this.db
      .prepare(
        "SELECT * FROM aptos_wallets WHERE user_id = ? ORDER BY created_at DESC",
      )
      .all(userId) as any[];
    return rows.map((r) => this.rowToRecord(r));
  }

  getAllWallets(): AptosWalletRecord[] {
    const rows = this.db
      .prepare("SELECT * FROM aptos_wallets ORDER BY created_at DESC")
      .all() as any[];
    return rows.map((r) => this.rowToRecord(r));
  }

  getPrimaryWallet(userId: string): AptosWalletRecord | undefined {
    const row = this.db
      .prepare(
        "SELECT * FROM aptos_wallets WHERE user_id = ? AND is_primary = 1 LIMIT 1",
      )
      .get(userId) as any;
    return row ? this.rowToRecord(row) : undefined;
  }

  setPrimaryWallet(userId: string, walletId: string): void {
    const clear = this.db.prepare(
      "UPDATE aptos_wallets SET is_primary = 0 WHERE user_id = ?",
    );
    const set = this.db.prepare(
      "UPDATE aptos_wallets SET is_primary = 1 WHERE wallet_id = ? AND user_id = ?",
    );
    const tx = this.db.transaction(() => {
      clear.run(userId);
      set.run(walletId, userId);
    });
    tx();
  }

  updateWalletLastUsed(walletId: string): void {
    this.db
      .prepare("UPDATE aptos_wallets SET last_used = ? WHERE wallet_id = ?")
      .run(Date.now(), walletId);
  }

  updateWalletLabel(walletId: string, label: string): void {
    this.db
      .prepare("UPDATE aptos_wallets SET label = ? WHERE wallet_id = ?")
      .run(label, walletId);
  }

  deleteWallet(walletId: string, userId: string): boolean {
    const result = this.db
      .prepare("DELETE FROM aptos_wallets WHERE wallet_id = ? AND user_id = ?")
      .run(walletId, userId);
    return result.changes > 0;
  }

  walletExists(walletId: string): boolean {
    return !!this.db
      .prepare("SELECT wallet_id FROM aptos_wallets WHERE wallet_id = ?")
      .get(walletId);
  }

  addressExists(accountAddress: string): boolean {
    return !!this.db
      .prepare("SELECT wallet_id FROM aptos_wallets WHERE account_address = ?")
      .get(accountAddress);
  }

  recordTransaction(params: {
    wallet_id: string;
    user_id: string;
    from_address: string;
    to_address: string;
    transaction_hash: string;
    tx_type: string;
    amount_octas?: number;
    coin_type?: string;
    metadata_address?: string;
    pool_address?: string;
    collection_name?: string;
    token_address?: string;
    gas_used: number;
    gas_unit_price: number;
    version: string;
    vm_status: string;
    success: boolean;
    network: string;
  }): void {
    const txId = `aptos_tx_${Date.now()}_${Math.random().toString(36).substring(7)}`;

    try {
      this.db
        .prepare(
          `
          INSERT INTO aptos_transactions (
            tx_id, wallet_id, user_id, from_address, to_address,
            transaction_hash, tx_type, amount_octas, coin_type,
            metadata_address, pool_address, collection_name, token_address,
            gas_used, gas_unit_price, version, vm_status, success,
            network, created_at
          ) VALUES (
            @tx_id, @wallet_id, @user_id, @from_address, @to_address,
            @transaction_hash, @tx_type, @amount_octas, @coin_type,
            @metadata_address, @pool_address, @collection_name, @token_address,
            @gas_used, @gas_unit_price, @version, @vm_status, @success,
            @network, @created_at
          )
        `,
        )
        .run({
          tx_id: txId,
          wallet_id: params.wallet_id,
          user_id: params.user_id,
          from_address: params.from_address,
          to_address: params.to_address,
          transaction_hash: params.transaction_hash,
          tx_type: params.tx_type,
          amount_octas: params.amount_octas ?? 0,
          coin_type: params.coin_type ?? null,
          metadata_address: params.metadata_address ?? null,
          pool_address: params.pool_address ?? null,
          collection_name: params.collection_name ?? null,
          token_address: params.token_address ?? null,
          gas_used: params.gas_used,
          gas_unit_price: params.gas_unit_price,
          version: params.version,
          vm_status: params.vm_status,
          success: params.success ? 1 : 0,
          network: params.network,
          created_at: Date.now(),
        });
    } catch (error) {
      logger.error("Failed to record Aptos transaction:", error);
    }
  }

  getWalletTransactions(
    walletId: string,
    limit: number = 20,
    offset: number = 0,
  ): AptosTransactionRecord[] {
    const rows = this.db
      .prepare(
        `SELECT * FROM aptos_transactions
         WHERE wallet_id = ?
         ORDER BY created_at DESC
         LIMIT ? OFFSET ?`,
      )
      .all(walletId, limit, offset) as any[];
    return rows.map((r) => this.rowToTransactionRecord(r));
  }

  getUserTransactions(
    userId: string,
    limit: number = 20,
    offset: number = 0,
  ): AptosTransactionRecord[] {
    const rows = this.db
      .prepare(
        `SELECT * FROM aptos_transactions
         WHERE user_id = ?
         ORDER BY created_at DESC
         LIMIT ? OFFSET ?`,
      )
      .all(userId, limit, offset) as any[];
    return rows.map((r) => this.rowToTransactionRecord(r));
  }

  getTransactionByHash(
    transactionHash: string,
  ): AptosTransactionRecord | undefined {
    const row = this.db
      .prepare("SELECT * FROM aptos_transactions WHERE transaction_hash = ?")
      .get(transactionHash) as any;
    return row ? this.rowToTransactionRecord(row) : undefined;
  }

  countWalletTransactions(walletId: string): number {
    const row = this.db
      .prepare(
        "SELECT COUNT(*) as count FROM aptos_transactions WHERE wallet_id = ?",
      )
      .get(walletId) as any;
    return row.count;
  }

  countUserTransactions(userId: string): number {
    const row = this.db
      .prepare(
        "SELECT COUNT(*) as count FROM aptos_transactions WHERE user_id = ?",
      )
      .get(userId) as any;
    return row.count;
  }

  getTransactionsByType(
    walletId: string,
    txType: string,
    limit: number = 20,
  ): AptosTransactionRecord[] {
    const rows = this.db
      .prepare(
        `SELECT * FROM aptos_transactions
         WHERE wallet_id = ? AND tx_type = ?
         ORDER BY created_at DESC
         LIMIT ?`,
      )
      .all(walletId, txType, limit) as any[];
    return rows.map((r) => this.rowToTransactionRecord(r));
  }

  private rowToRecord(row: any): AptosWalletRecord {
    return {
      ...row,
      label: row.label ?? undefined,
      last_used: row.last_used ?? undefined,
      is_primary: row.is_primary === 1,
    };
  }

  private rowToTransactionRecord(row: any): AptosTransactionRecord {
    return {
      tx_id: row.tx_id,
      wallet_id: row.wallet_id,
      user_id: row.user_id,
      from_address: row.from_address,
      to_address: row.to_address,
      transaction_hash: row.transaction_hash,
      tx_type: row.tx_type,
      amount_octas: row.amount_octas ?? 0,
      coin_type: row.coin_type ?? undefined,
      metadata_address: row.metadata_address ?? undefined,
      pool_address: row.pool_address ?? undefined,
      collection_name: row.collection_name ?? undefined,
      token_address: row.token_address ?? undefined,
      gas_used: row.gas_used ?? 0,
      gas_unit_price: row.gas_unit_price ?? 0,
      version: row.version ?? "",
      vm_status: row.vm_status ?? "",
      success: row.success === 1,
      network: row.network ?? "testnet",
      created_at: row.created_at,
    };
  }
}

export const aptosDatabase = new AptosDatabase();
