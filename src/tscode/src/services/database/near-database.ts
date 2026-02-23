import Database from "better-sqlite3";
import path from "path";
import fs from "fs";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";

const DB_PATH = process.env.NEAR_DB_PATH || "./data/near-wallets.db";

export class NearDatabase {
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
    logger.info("NEAR Database initialized at:", DB_PATH);
  }

  private initializeDatabase(): void {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS near_wallets (
        wallet_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        account_id TEXT NOT NULL UNIQUE,
        public_key TEXT NOT NULL,
        encrypted_private_key TEXT NOT NULL,
        encryption_iv TEXT NOT NULL,
        encryption_tag TEXT NOT NULL,
        account_name TEXT,
        account_type TEXT NOT NULL DEFAULT 'named',
        parent_account_id TEXT,
        network TEXT NOT NULL DEFAULT 'testnet',
        created_at INTEGER NOT NULL,
        last_used INTEGER,
        is_deleted INTEGER NOT NULL DEFAULT 0,
        deleted_at INTEGER,
        beneficiary_account_id TEXT
      );

      CREATE INDEX IF NOT EXISTS idx_near_wallet_user ON near_wallets(user_id);
      CREATE INDEX IF NOT EXISTS idx_near_wallet_account ON near_wallets(account_id);
      CREATE INDEX IF NOT EXISTS idx_near_wallet_network ON near_wallets(network);
      CREATE INDEX IF NOT EXISTS idx_near_wallet_type ON near_wallets(account_type);
      CREATE INDEX IF NOT EXISTS idx_near_wallet_parent ON near_wallets(parent_account_id);

      CREATE TABLE IF NOT EXISTS near_access_keys (
        key_id TEXT PRIMARY KEY,
        wallet_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        account_id TEXT NOT NULL,
        public_key TEXT NOT NULL,
        encrypted_private_key TEXT NOT NULL,
        encryption_iv TEXT NOT NULL,
        encryption_tag TEXT NOT NULL,
        key_type TEXT NOT NULL DEFAULT 'full_access',
        contract_id TEXT,
        method_names TEXT,
        allowance_near TEXT,
        label TEXT,
        created_at INTEGER NOT NULL,
        is_active INTEGER NOT NULL DEFAULT 1
      );

      CREATE INDEX IF NOT EXISTS idx_near_key_wallet ON near_access_keys(wallet_id);
      CREATE INDEX IF NOT EXISTS idx_near_key_user ON near_access_keys(user_id);
      CREATE INDEX IF NOT EXISTS idx_near_key_account ON near_access_keys(account_id);
      CREATE INDEX IF NOT EXISTS idx_near_key_active ON near_access_keys(is_active);

      CREATE TABLE IF NOT EXISTS near_transactions (
        tx_id TEXT PRIMARY KEY,
        wallet_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        from_account_id TEXT NOT NULL,
        receiver_id TEXT NOT NULL,
        transaction_hash TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'pending',
        actions_count INTEGER NOT NULL DEFAULT 1,
        tx_type TEXT NOT NULL DEFAULT 'transfer',
        contract_id TEXT,
        method_name TEXT,
        amount_near TEXT,
        gas_tera TEXT,
        created_at INTEGER NOT NULL
      );

      CREATE INDEX IF NOT EXISTS idx_near_tx_wallet ON near_transactions(wallet_id);
      CREATE INDEX IF NOT EXISTS idx_near_tx_user ON near_transactions(user_id);
      CREATE INDEX IF NOT EXISTS idx_near_tx_hash ON near_transactions(transaction_hash);
      CREATE INDEX IF NOT EXISTS idx_near_tx_created ON near_transactions(created_at);

      CREATE TABLE IF NOT EXISTS near_schema_migrations (
        id INTEGER PRIMARY KEY,
        applied_at INTEGER NOT NULL
      );
    `);
  }

  private runMigrations(): void {
    const migrations: { id: number; sql: string }[] = [];

    for (const migration of migrations) {
      const already = this.db
        .prepare("SELECT id FROM near_schema_migrations WHERE id = ?")
        .get(migration.id);

      if (!already) {
        try {
          this.db.exec(migration.sql);
          logger.info(`NEAR Migration ${migration.id} applied`);
        } catch {
          logger.info(`NEAR Migration ${migration.id} skipped`);
        }
        this.db
          .prepare(
            "INSERT INTO near_schema_migrations (id, applied_at) VALUES (?, ?)",
          )
          .run(migration.id, Date.now());
      }
    }
  }

  createWallet(wallet: {
    wallet_id: string;
    user_id: string;
    account_id: string;
    public_key: string;
    encrypted_private_key: string;
    encryption_iv: string;
    encryption_tag: string;
    account_name?: string;
    account_type: "named" | "subaccount" | "implicit";
    parent_account_id?: string;
    network: string;
    created_at: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO near_wallets (
        wallet_id, user_id, account_id, public_key,
        encrypted_private_key, encryption_iv, encryption_tag,
        account_name, account_type, parent_account_id, network, created_at
      ) VALUES (
        @wallet_id, @user_id, @account_id, @public_key,
        @encrypted_private_key, @encryption_iv, @encryption_tag,
        @account_name, @account_type, @parent_account_id, @network, @created_at
      )
    `);
    stmt.run({
      ...wallet,
      account_name: wallet.account_name ?? null,
      parent_account_id: wallet.parent_account_id ?? null,
    });
  }

  getWallet(walletId: string): any {
    return this.db
      .prepare(
        "SELECT * FROM near_wallets WHERE wallet_id = ? AND is_deleted = 0",
      )
      .get(walletId);
  }

  getWalletByAccountId(accountId: string): any {
    return this.db
      .prepare(
        "SELECT * FROM near_wallets WHERE account_id = ? AND is_deleted = 0",
      )
      .get(accountId);
  }

  getUserWallets(userId: string): any[] {
    return this.db
      .prepare(
        "SELECT * FROM near_wallets WHERE user_id = ? AND is_deleted = 0 ORDER BY created_at DESC",
      )
      .all(userId);
  }

  getSubAccounts(parentAccountId: string): any[] {
    return this.db
      .prepare(
        "SELECT * FROM near_wallets WHERE parent_account_id = ? AND is_deleted = 0 ORDER BY created_at DESC",
      )
      .all(parentAccountId);
  }

  getAllWallets(): any[] {
    return this.db
      .prepare(
        "SELECT * FROM near_wallets WHERE is_deleted = 0 ORDER BY created_at DESC",
      )
      .all();
  }

  updateWalletLastUsed(walletId: string): void {
    this.db
      .prepare("UPDATE near_wallets SET last_used = ? WHERE wallet_id = ?")
      .run(Date.now(), walletId);
  }

  markWalletDeleted(
    walletId: string,
    userId: string,
    beneficiaryAccountId: string,
  ): boolean {
    const result = this.db
      .prepare(
        "UPDATE near_wallets SET is_deleted = 1, deleted_at = ?, beneficiary_account_id = ? WHERE wallet_id = ? AND user_id = ?",
      )
      .run(Date.now(), beneficiaryAccountId, walletId, userId);
    return result.changes > 0;
  }

  hardDeleteWallet(walletId: string, userId: string): boolean {
    const result = this.db
      .prepare("DELETE FROM near_wallets WHERE wallet_id = ? AND user_id = ?")
      .run(walletId, userId);
    return result.changes > 0;
  }

  walletExists(accountId: string): boolean {
    return !!this.db
      .prepare(
        "SELECT wallet_id FROM near_wallets WHERE account_id = ? AND is_deleted = 0",
      )
      .get(accountId);
  }

  createAccessKey(params: {
    wallet_id: string;
    user_id: string;
    account_id: string;
    public_key: string;
    private_key_plain: string;
    key_type: "full_access" | "function_call";
    contract_id?: string;
    method_names?: string[];
    allowance_near?: string;
    label?: string;
  }): void {
    const privateKeyHex = Buffer.from(params.private_key_plain).toString("hex");
    const encrypted = encryptionService.encrypt(privateKeyHex);
    const keyId = `near_key_${Date.now()}_${Math.random().toString(36).substring(7)}`;

    this.db
      .prepare(
        `
        INSERT INTO near_access_keys (
          key_id, wallet_id, user_id, account_id, public_key,
          encrypted_private_key, encryption_iv, encryption_tag,
          key_type, contract_id, method_names, allowance_near, label, created_at
        ) VALUES (
          @key_id, @wallet_id, @user_id, @account_id, @public_key,
          @encrypted_private_key, @encryption_iv, @encryption_tag,
          @key_type, @contract_id, @method_names, @allowance_near, @label, @created_at
        )
      `,
      )
      .run({
        key_id: keyId,
        wallet_id: params.wallet_id,
        user_id: params.user_id,
        account_id: params.account_id,
        public_key: params.public_key,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        key_type: params.key_type,
        contract_id: params.contract_id ?? null,
        method_names: params.method_names
          ? JSON.stringify(params.method_names)
          : null,
        allowance_near: params.allowance_near ?? null,
        label: params.label ?? null,
        created_at: Date.now(),
      });
  }

  getAccessKeys(walletId: string): any[] {
    const rows = this.db
      .prepare(
        "SELECT * FROM near_access_keys WHERE wallet_id = ? AND is_active = 1 ORDER BY created_at DESC",
      )
      .all(walletId);
    return rows.map((r: any) => ({
      ...r,
      method_names: r.method_names ? JSON.parse(r.method_names) : [],
    }));
  }

  deactivateAccessKey(walletId: string, publicKey: string): void {
    this.db
      .prepare(
        "UPDATE near_access_keys SET is_active = 0 WHERE wallet_id = ? AND public_key = ?",
      )
      .run(walletId, publicKey);
  }

  createTransactionRecord(params: {
    wallet_id: string;
    user_id: string;
    from_account_id: string;
    receiver_id: string;
    transaction_hash: string;
    status: string;
    actions_count: number;
    tx_type: string;
    contract_id?: string;
    method_name?: string;
    amount_near?: string;
    gas_tera?: string;
  }): void {
    const txId = `near_tx_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    this.db
      .prepare(
        `
        INSERT INTO near_transactions (
          tx_id, wallet_id, user_id, from_account_id, receiver_id,
          transaction_hash, status, actions_count, tx_type,
          contract_id, method_name, amount_near, gas_tera, created_at
        ) VALUES (
          @tx_id, @wallet_id, @user_id, @from_account_id, @receiver_id,
          @transaction_hash, @status, @actions_count, @tx_type,
          @contract_id, @method_name, @amount_near, @gas_tera, @created_at
        )
      `,
      )
      .run({
        tx_id: txId,
        wallet_id: params.wallet_id,
        user_id: params.user_id,
        from_account_id: params.from_account_id,
        receiver_id: params.receiver_id,
        transaction_hash: params.transaction_hash,
        status: params.status,
        actions_count: params.actions_count,
        tx_type: params.tx_type,
        contract_id: params.contract_id ?? null,
        method_name: params.method_name ?? null,
        amount_near: params.amount_near ?? null,
        gas_tera: params.gas_tera ?? null,
        created_at: Date.now(),
      });
  }

  getTransactionRecords(
    walletId: string,
    limit: number,
    offset: number,
  ): any[] {
    return this.db
      .prepare(
        "SELECT * FROM near_transactions WHERE wallet_id = ? ORDER BY created_at DESC LIMIT ? OFFSET ?",
      )
      .all(walletId, limit, offset);
  }

  countTransactionRecords(walletId: string): number {
    const row = this.db
      .prepare(
        "SELECT COUNT(*) as count FROM near_transactions WHERE wallet_id = ?",
      )
      .get(walletId) as any;
    return row.count;
  }

  getTransactionByHash(txHash: string): any {
    return this.db
      .prepare("SELECT * FROM near_transactions WHERE transaction_hash = ?")
      .get(txHash);
  }
}

export const nearDatabase = new NearDatabase();
