import Database from "better-sqlite3";
import path from "path";
import fs from "fs";
import { logger } from "../../core/logger.js";

const DB_PATH = process.env.DB_PATH || "./data/wallets.db";

export class WalletDatabase {
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
    logger.info("Database initialized at:", DB_PATH);
  }

  private initializeDatabase(): void {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS wallets (
        wallet_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        label TEXT,
        public_key TEXT NOT NULL UNIQUE,
        encrypted_private_key TEXT NOT NULL,
        encryption_iv TEXT NOT NULL,
        encryption_tag TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        last_used INTEGER
      );

      CREATE INDEX IF NOT EXISTS idx_wallet_user ON wallets(user_id);
      CREATE INDEX IF NOT EXISTS idx_wallet_public_key ON wallets(public_key);

      CREATE TABLE IF NOT EXISTS users (
        user_id TEXT PRIMARY KEY,
        username TEXT NOT NULL UNIQUE,
        password_hash TEXT NOT NULL,
        email TEXT,
        created_at INTEGER NOT NULL,
        last_login INTEGER,
        is_locked INTEGER DEFAULT 0,
        locked_until INTEGER,
        failed_login_attempts INTEGER DEFAULT 0,
        two_fa_enabled INTEGER DEFAULT 0,
        two_fa_secret TEXT,
        password_changed_at INTEGER
      );

      CREATE INDEX IF NOT EXISTS idx_user_username ON users(username);

      CREATE TABLE IF NOT EXISTS sessions (
        session_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        token_hash TEXT NOT NULL UNIQUE,
        ip_address TEXT,
        user_agent TEXT,
        created_at INTEGER NOT NULL,
        expires_at INTEGER NOT NULL,
        last_activity INTEGER NOT NULL,
        is_active INTEGER DEFAULT 1
      );

      CREATE INDEX IF NOT EXISTS idx_session_user ON sessions(user_id);
      CREATE INDEX IF NOT EXISTS idx_session_token ON sessions(token_hash);
      CREATE INDEX IF NOT EXISTS idx_session_active ON sessions(is_active);

      CREATE TABLE IF NOT EXISTS login_attempts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT NOT NULL,
        ip_address TEXT,
        user_agent TEXT,
        success INTEGER NOT NULL,
        timestamp INTEGER NOT NULL,
        failure_reason TEXT
      );

      CREATE INDEX IF NOT EXISTS idx_login_username ON login_attempts(username);
      CREATE INDEX IF NOT EXISTS idx_login_ip ON login_attempts(ip_address);
      CREATE INDEX IF NOT EXISTS idx_login_timestamp ON login_attempts(timestamp);

      CREATE TABLE IF NOT EXISTS blocked_ips (
        ip_address TEXT PRIMARY KEY,
        reason TEXT,
        blocked_at INTEGER NOT NULL,
        blocked_until INTEGER NOT NULL,
        block_count INTEGER DEFAULT 1
      );

      CREATE TABLE IF NOT EXISTS api_keys (
        key_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        key_hash TEXT NOT NULL UNIQUE,
        label TEXT,
        permissions TEXT,
        created_at INTEGER NOT NULL,
        expires_at INTEGER,
        last_used INTEGER,
        is_active INTEGER DEFAULT 1
      );

      CREATE INDEX IF NOT EXISTS idx_api_key_user ON api_keys(user_id);
      CREATE INDEX IF NOT EXISTS idx_api_key_hash ON api_keys(key_hash);

      CREATE TABLE IF NOT EXISTS password_reset_tokens (
        token_id TEXT PRIMARY KEY,
        user_id TEXT NOT NULL,
        token_hash TEXT NOT NULL UNIQUE,
        created_at INTEGER NOT NULL,
        expires_at INTEGER NOT NULL,
        used INTEGER DEFAULT 0
      );

      CREATE TABLE IF NOT EXISTS airdrops (
        airdrop_id TEXT PRIMARY KEY,
        wallet_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        type TEXT NOT NULL,
        token_mint TEXT,
        total_recipients INTEGER NOT NULL,
        successful INTEGER DEFAULT 0,
        failed INTEGER DEFAULT 0,
        total_amount INTEGER,
        status TEXT NOT NULL DEFAULT 'processing',
        created_at INTEGER NOT NULL,
        completed_at INTEGER
      );

      CREATE INDEX IF NOT EXISTS idx_airdrop_wallet ON airdrops(wallet_id);
      CREATE INDEX IF NOT EXISTS idx_airdrop_user ON airdrops(user_id);
      CREATE INDEX IF NOT EXISTS idx_airdrop_status ON airdrops(status);
      CREATE INDEX IF NOT EXISTS idx_airdrop_created ON airdrops(created_at);

      CREATE TABLE IF NOT EXISTS airdrop_recipients (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        airdrop_id TEXT NOT NULL,
        recipient_address TEXT NOT NULL,
        amount INTEGER,
        mint_address TEXT,
        success INTEGER NOT NULL DEFAULT 0,
        signature TEXT,
        error TEXT,
        processed_at INTEGER
      );

      CREATE INDEX IF NOT EXISTS idx_airdrop_recipient_airdrop ON airdrop_recipients(airdrop_id);
      CREATE INDEX IF NOT EXISTS idx_airdrop_recipient_address ON airdrop_recipients(recipient_address);
      CREATE INDEX IF NOT EXISTS idx_airdrop_recipient_success ON airdrop_recipients(success);

      CREATE TABLE IF NOT EXISTS stake_accounts (
        stake_id TEXT PRIMARY KEY,
        wallet_id TEXT NOT NULL,
        user_id TEXT NOT NULL,
        stake_account_address TEXT NOT NULL UNIQUE,
        validator_vote_address TEXT NOT NULL,
        amount_lamports INTEGER NOT NULL,
        status TEXT NOT NULL DEFAULT 'created',
        signature TEXT NOT NULL,
        created_at INTEGER NOT NULL,
        delegated_at INTEGER,
        deactivated_at INTEGER,
        withdrawn_at INTEGER
      );

      CREATE INDEX IF NOT EXISTS idx_stake_wallet ON stake_accounts(wallet_id);
      CREATE INDEX IF NOT EXISTS idx_stake_user ON stake_accounts(user_id);
      CREATE INDEX IF NOT EXISTS idx_stake_status ON stake_accounts(status);
      CREATE INDEX IF NOT EXISTS idx_stake_address ON stake_accounts(stake_account_address);

      CREATE TABLE IF NOT EXISTS schema_migrations (
        id INTEGER PRIMARY KEY,
        applied_at INTEGER NOT NULL
      );
    `);
  }

  private runMigrations(): void {
    const migrations = [
      {
        id: 1,
        sql: `ALTER TABLE login_attempts ADD COLUMN failure_reason TEXT`,
      },
      {
        id: 2,
        sql: `ALTER TABLE users ADD COLUMN locked_until INTEGER`,
      },
      {
        id: 3,
        sql: `ALTER TABLE users ADD COLUMN password_changed_at INTEGER`,
      },
      {
        id: 4,
        sql: `ALTER TABLE sessions ADD COLUMN user_agent TEXT`,
      },
      {
        id: 5,
        sql: `ALTER TABLE login_attempts ADD COLUMN user_agent TEXT`,
      },
    ];

    for (const migration of migrations) {
      const already = this.db
        .prepare("SELECT id FROM schema_migrations WHERE id = ?")
        .get(migration.id);

      if (!already) {
        try {
          this.db.exec(migration.sql);
          logger.info(`Migration ${migration.id} applied`);
        } catch {
          logger.info(
            `Migration ${migration.id} skipped (column may already exist)`,
          );
        }

        this.db
          .prepare(
            "INSERT INTO schema_migrations (id, applied_at) VALUES (?, ?)",
          )
          .run(migration.id, Date.now());
      }
    }
  }

  createWallet(wallet: {
    wallet_id: string;
    user_id: string;
    label?: string;
    public_key: string;
    encrypted_private_key: string;
    encryption_iv: string;
    encryption_tag: string;
    created_at: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO wallets (
        wallet_id, user_id, label, public_key,
        encrypted_private_key, encryption_iv, encryption_tag, created_at
      ) VALUES (
        @wallet_id, @user_id, @label, @public_key,
        @encrypted_private_key, @encryption_iv, @encryption_tag, @created_at
      )
    `);
    stmt.run(wallet);
  }

  getWallet(walletId: string): any {
    const stmt = this.db.prepare("SELECT * FROM wallets WHERE wallet_id = ?");
    return stmt.get(walletId);
  }

  getWalletByPublicKey(publicKey: string): any {
    const stmt = this.db.prepare("SELECT * FROM wallets WHERE public_key = ?");
    return stmt.get(publicKey);
  }

  getUserWallets(userId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM wallets WHERE user_id = ? ORDER BY created_at DESC",
    );
    return stmt.all(userId);
  }

  updateWalletLastUsed(walletId: string): void {
    const stmt = this.db.prepare(
      "UPDATE wallets SET last_used = ? WHERE wallet_id = ?",
    );
    stmt.run(Date.now(), walletId);
  }

  deleteWallet(walletId: string): void {
    const stmt = this.db.prepare("DELETE FROM wallets WHERE wallet_id = ?");
    stmt.run(walletId);
  }

  createUser(user: {
    user_id: string;
    username: string;
    password_hash: string;
    email?: string;
    created_at: number;
    last_login?: number;
    is_locked?: number;
    failed_login_attempts?: number;
    two_fa_enabled?: number;
    password_changed_at?: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO users (
        user_id, username, password_hash, email, created_at,
        last_login, is_locked, failed_login_attempts, two_fa_enabled, password_changed_at
      ) VALUES (
        @user_id, @username, @password_hash, @email, @created_at,
        @last_login, @is_locked, @failed_login_attempts, @two_fa_enabled, @password_changed_at
      )
    `);
    stmt.run({
      ...user,
      last_login: user.last_login ?? null,
      is_locked: user.is_locked ?? 0,
      failed_login_attempts: user.failed_login_attempts ?? 0,
      two_fa_enabled: user.two_fa_enabled ?? 0,
      password_changed_at: user.password_changed_at ?? Date.now(),
    });
  }

  getUser(userId: string): any {
    const stmt = this.db.prepare("SELECT * FROM users WHERE user_id = ?");
    return stmt.get(userId);
  }

  getUserByUsername(username: string): any {
    const stmt = this.db.prepare("SELECT * FROM users WHERE username = ?");
    return stmt.get(username);
  }

  updateUser(userId: string, updates: Record<string, any>): void {
    const fields = Object.keys(updates)
      .map((key) => `${key} = @${key}`)
      .join(", ");
    const stmt = this.db.prepare(
      `UPDATE users SET ${fields} WHERE user_id = @user_id`,
    );
    stmt.run({ ...updates, user_id: userId });
  }

  updateUserLastLogin(userId: string): void {
    const stmt = this.db.prepare(
      "UPDATE users SET last_login = ? WHERE user_id = ?",
    );
    stmt.run(Date.now(), userId);
  }

  incrementFailedLoginAttempts(userId: string): number {
    const stmt = this.db.prepare(`
      UPDATE users SET failed_login_attempts = failed_login_attempts + 1
      WHERE user_id = ?
    `);
    stmt.run(userId);
    const user = this.getUser(userId);
    return user?.failed_login_attempts || 0;
  }

  resetFailedLoginAttempts(userId: string): void {
    const stmt = this.db.prepare(
      "UPDATE users SET failed_login_attempts = 0 WHERE user_id = ?",
    );
    stmt.run(userId);
  }

  lockUser(userId: string, duration: number): void {
    const stmt = this.db.prepare(
      "UPDATE users SET is_locked = 1, locked_until = ? WHERE user_id = ?",
    );
    stmt.run(Date.now() + duration, userId);
  }

  unlockUser(userId: string): void {
    const stmt = this.db.prepare(
      "UPDATE users SET is_locked = 0, locked_until = NULL, failed_login_attempts = 0 WHERE user_id = ?",
    );
    stmt.run(userId);
  }

  createSession(session: {
    session_id: string;
    user_id: string;
    token_hash: string;
    ip_address?: string;
    user_agent?: string;
    created_at: number;
    expires_at: number;
    last_activity: number;
    is_active?: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO sessions (
        session_id, user_id, token_hash, ip_address, user_agent,
        created_at, expires_at, last_activity, is_active
      ) VALUES (
        @session_id, @user_id, @token_hash, @ip_address, @user_agent,
        @created_at, @expires_at, @last_activity, @is_active
      )
    `);
    stmt.run({ ...session, is_active: session.is_active ?? 1 });
  }

  getSessionByTokenHash(tokenHash: string): any {
    const stmt = this.db.prepare("SELECT * FROM sessions WHERE token_hash = ?");
    return stmt.get(tokenHash);
  }

  getSession(sessionId: string): any {
    const stmt = this.db.prepare("SELECT * FROM sessions WHERE session_id = ?");
    return stmt.get(sessionId);
  }

  getUserSessions(userId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM sessions WHERE user_id = ? AND is_active = 1 ORDER BY last_activity DESC",
    );
    return stmt.all(userId);
  }

  updateSessionActivity(sessionId: string): void {
    const stmt = this.db.prepare(
      "UPDATE sessions SET last_activity = ? WHERE session_id = ?",
    );
    stmt.run(Date.now(), sessionId);
  }

  invalidateSession(sessionId: string): void {
    const stmt = this.db.prepare(
      "UPDATE sessions SET is_active = 0 WHERE session_id = ?",
    );
    stmt.run(sessionId);
  }

  invalidateAllUserSessions(userId: string): void {
    const stmt = this.db.prepare(
      "UPDATE sessions SET is_active = 0 WHERE user_id = ?",
    );
    stmt.run(userId);
  }

  cleanupExpiredSessions(): void {
    const stmt = this.db.prepare(
      "UPDATE sessions SET is_active = 0 WHERE expires_at < ? AND is_active = 1",
    );
    stmt.run(Date.now());
  }

  logLoginAttempt(attempt: {
    username: string;
    ip_address?: string;
    user_agent?: string;
    success: number;
    timestamp: number;
    failure_reason?: string;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO login_attempts (username, ip_address, user_agent, success, timestamp, failure_reason)
      VALUES (@username, @ip_address, @user_agent, @success, @timestamp, @failure_reason)
    `);
    stmt.run({
      username: attempt.username,
      ip_address: attempt.ip_address ?? null,
      user_agent: attempt.user_agent ?? null,
      success: attempt.success,
      timestamp: attempt.timestamp,
      failure_reason: attempt.failure_reason ?? null,
    });
  }

  getRecentLoginAttempts(username: string, minutes: number): any[] {
    const since = Date.now() - minutes * 60 * 1000;
    const stmt = this.db.prepare(
      "SELECT * FROM login_attempts WHERE username = ? AND timestamp > ? ORDER BY timestamp DESC",
    );
    return stmt.all(username, since);
  }

  getRecentIPAttempts(ipAddress: string, minutes: number): any[] {
    const since = Date.now() - minutes * 60 * 1000;
    const stmt = this.db.prepare(
      "SELECT * FROM login_attempts WHERE ip_address = ? AND timestamp > ? ORDER BY timestamp DESC",
    );
    return stmt.all(ipAddress, since);
  }

  blockIP(blocked: {
    ip_address: string;
    reason: string;
    blocked_at: number;
    blocked_until: number;
    block_count: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO blocked_ips (ip_address, reason, blocked_at, blocked_until, block_count)
      VALUES (@ip_address, @reason, @blocked_at, @blocked_until, @block_count)
      ON CONFLICT(ip_address) DO UPDATE SET
        blocked_until = @blocked_until,
        block_count = block_count + 1,
        blocked_at = @blocked_at
    `);
    stmt.run(blocked);
  }

  isIPBlocked(ipAddress: string): boolean {
    const stmt = this.db.prepare(
      "SELECT * FROM blocked_ips WHERE ip_address = ? AND blocked_until > ?",
    );
    const result = stmt.get(ipAddress, Date.now());
    return !!result;
  }

  createAirdrop(airdrop: {
    airdrop_id: string;
    wallet_id: string;
    user_id: string;
    type: string;
    token_mint?: string;
    total_recipients: number;
    successful?: number;
    failed?: number;
    total_amount?: number;
    status: string;
    created_at: number;
    completed_at?: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO airdrops (
        airdrop_id, wallet_id, user_id, type, token_mint,
        total_recipients, successful, failed, total_amount, status, created_at, completed_at
      ) VALUES (
        @airdrop_id, @wallet_id, @user_id, @type, @token_mint,
        @total_recipients, @successful, @failed, @total_amount, @status, @created_at, @completed_at
      )
    `);
    stmt.run({
      ...airdrop,
      token_mint: airdrop.token_mint ?? null,
      successful: airdrop.successful ?? 0,
      failed: airdrop.failed ?? 0,
      total_amount: airdrop.total_amount ?? null,
      completed_at: airdrop.completed_at ?? null,
    });
  }

  getAirdrop(airdropId: string): any {
    const stmt = this.db.prepare("SELECT * FROM airdrops WHERE airdrop_id = ?");
    return stmt.get(airdropId);
  }

  updateAirdrop(airdropId: string, updates: Record<string, any>): void {
    const fields = Object.keys(updates)
      .map((key) => `${key} = @${key}`)
      .join(", ");
    const stmt = this.db.prepare(
      `UPDATE airdrops SET ${fields} WHERE airdrop_id = @airdrop_id`,
    );
    stmt.run({ ...updates, airdrop_id: airdropId });
  }

  getUserAirdrops(userId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM airdrops WHERE user_id = ? ORDER BY created_at DESC",
    );
    return stmt.all(userId);
  }

  getWalletAirdrops(walletId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM airdrops WHERE wallet_id = ? ORDER BY created_at DESC",
    );
    return stmt.all(walletId);
  }

  createAirdropRecipient(recipient: {
    airdrop_id: string;
    recipient_address: string;
    amount?: number;
    mint_address?: string;
    success: boolean;
    signature?: string;
    error?: string;
    processed_at: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO airdrop_recipients (
        airdrop_id, recipient_address, amount, mint_address,
        success, signature, error, processed_at
      ) VALUES (
        @airdrop_id, @recipient_address, @amount, @mint_address,
        @success, @signature, @error, @processed_at
      )
    `);
    stmt.run({
      ...recipient,
      amount: recipient.amount ?? null,
      mint_address: recipient.mint_address ?? null,
      success: recipient.success ? 1 : 0,
      signature: recipient.signature ?? null,
      error: recipient.error ?? null,
    });
  }

  getAirdropRecipients(airdropId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM airdrop_recipients WHERE airdrop_id = ?",
    );
    return stmt.all(airdropId);
  }

  getAirdropStats(airdropId: string): any {
    const stmt = this.db.prepare(`
      SELECT
        COUNT(*) as total,
        SUM(CASE WHEN success = 1 THEN 1 ELSE 0 END) as successful,
        SUM(CASE WHEN success = 0 THEN 1 ELSE 0 END) as failed
      FROM airdrop_recipients WHERE airdrop_id = ?
    `);
    return stmt.get(airdropId);
  }

  createStakeAccount(stake: {
    stake_id: string;
    wallet_id: string;
    user_id: string;
    stake_account_address: string;
    validator_vote_address: string;
    amount_lamports: number;
    status: string;
    signature: string;
    created_at: number;
  }): void {
    const stmt = this.db.prepare(`
      INSERT INTO stake_accounts (
        stake_id, wallet_id, user_id, stake_account_address,
        validator_vote_address, amount_lamports, status, signature, created_at
      ) VALUES (
        @stake_id, @wallet_id, @user_id, @stake_account_address,
        @validator_vote_address, @amount_lamports, @status, @signature, @created_at
      )
    `);
    stmt.run(stake);
  }

  getStakeAccount(stakeId: string): any {
    const stmt = this.db.prepare(
      "SELECT * FROM stake_accounts WHERE stake_id = ?",
    );
    return stmt.get(stakeId);
  }

  getStakeAccountByAddress(address: string): any {
    const stmt = this.db.prepare(
      "SELECT * FROM stake_accounts WHERE stake_account_address = ?",
    );
    return stmt.get(address);
  }

  updateStakeAccount(
    stakeId: string,
    updates: {
      status?: string;
      delegated_at?: number;
      deactivated_at?: number;
      withdrawn_at?: number;
    },
  ): void {
    const fields = Object.keys(updates)
      .map((key) => `${key} = @${key}`)
      .join(", ");
    const stmt = this.db.prepare(
      `UPDATE stake_accounts SET ${fields} WHERE stake_id = @stake_id`,
    );
    stmt.run({ ...updates, stake_id: stakeId });
  }

  getUserStakeAccounts(userId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM stake_accounts WHERE user_id = ? ORDER BY created_at DESC",
    );
    return stmt.all(userId);
  }

  getWalletStakeAccounts(walletId: string): any[] {
    const stmt = this.db.prepare(
      "SELECT * FROM stake_accounts WHERE wallet_id = ? ORDER BY created_at DESC",
    );
    return stmt.all(walletId);
  }
}

export const walletDatabase = new WalletDatabase();
