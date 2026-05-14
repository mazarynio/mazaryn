import { Account, Ed25519PrivateKey } from "@aptos-labs/ts-sdk";
import { aptosConnection } from "./connection.js";
import { aptosDatabase } from "../database/aptos-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  CreateAptosAccountRequest,
  CreateAptosAccountResponse,
  ImportAptosAccountRequest,
  DeleteAptosAccountRequest,
  AptosAccountInfo,
  AptosBalanceResponse,
  AptosFABalanceResponse,
} from "../../core/aptos-types.js";

interface CachedAptosWallet {
  account: Account;
  address: string;
}

export class AptosAccountManager {
  private walletCache: Map<string, CachedAptosWallet> = new Map();

  constructor() {
    this.loadWalletsFromDatabase();
  }

  private async loadWalletsFromDatabase(): Promise<void> {
    try {
      const wallets = aptosDatabase.getAllWallets();
      logger.info(`Loading ${wallets.length} Aptos wallets from database`);

      for (const wallet of wallets) {
        try {
          const privateKeyHex = encryptionService.decrypt(
            wallet.encrypted_private_key,
            wallet.encryption_iv,
            wallet.encryption_tag,
          );

          const privateKey = new Ed25519PrivateKey(privateKeyHex);
          const account = Account.fromPrivateKey({ privateKey });

          this.walletCache.set(wallet.wallet_id, {
            account,
            address: wallet.account_address,
          });
        } catch (error) {
          logger.error(
            `Failed to load Aptos wallet ${wallet.wallet_id}:`,
            error,
          );
        }
      }

      logger.info(
        `Successfully loaded ${this.walletCache.size} Aptos wallets into cache`,
      );
    } catch (error) {
      logger.error("Failed to load Aptos wallets from database:", error);
    }
  }

  private generateWalletId(): string {
    return `aptos_wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  async createAccount(
    req: CreateAptosAccountRequest,
  ): Promise<CreateAptosAccountResponse> {
    try {
      logger.info("Creating new Aptos account for user:", req.user_id);

      const account = Account.generate();
      const address = account.accountAddress.toString();
      const publicKey = account.publicKey.toString();
      const privateKeyHex = account.privateKey.toString();

      const encrypted = encryptionService.encrypt(privateKeyHex);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      aptosDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        account_address: address,
        public_key: publicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        label: req.label,
        signing_scheme: "ed25519",
        network: config.aptosNetwork,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, { account, address });

      logger.info(`Aptos wallet created and persisted: ${walletId}`);

      return {
        account_address: address,
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: publicKey,
        signing_scheme: "ed25519",
        network: config.aptosNetwork,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to create Aptos account:", error);
      throw error;
    }
  }

  async importAccount(
    req: ImportAptosAccountRequest,
  ): Promise<CreateAptosAccountResponse> {
    try {
      logger.info("Importing Aptos account for user:", req.user_id);

      const privateKey = new Ed25519PrivateKey(req.private_key);
      const account = Account.fromPrivateKey({ privateKey });
      const address = account.accountAddress.toString();
      const publicKey = account.publicKey.toString();

      if (aptosDatabase.addressExists(address)) {
        throw new Error("Account already imported");
      }

      const encrypted = encryptionService.encrypt(req.private_key);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      aptosDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        account_address: address,
        public_key: publicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        label: req.label,
        signing_scheme: "ed25519",
        network: config.aptosNetwork,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, { account, address });

      logger.info(`Aptos wallet imported and persisted: ${walletId}`);

      return {
        account_address: address,
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: publicKey,
        signing_scheme: "ed25519",
        network: config.aptosNetwork,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to import Aptos account:", error);
      throw error;
    }
  }

  deleteAccount(req: DeleteAptosAccountRequest): boolean {
    try {
      logger.info(
        `Deleting Aptos wallet ${req.wallet_id} for user ${req.user_id}`,
      );

      const deleted = aptosDatabase.deleteWallet(req.wallet_id, req.user_id);
      if (deleted) {
        this.walletCache.delete(req.wallet_id);
        logger.info(`Aptos wallet deleted: ${req.wallet_id}`);
      }
      return deleted;
    } catch (error) {
      logger.error("Failed to delete Aptos account:", error);
      throw error;
    }
  }

  async getBalance(
    accountAddress: string,
    networkStr?: string,
  ): Promise<AptosBalanceResponse> {
    try {
      logger.info("Getting APT balance for:", accountAddress);

      const balanceOctas = await aptosConnection.getAccountAPTBalance(
        accountAddress,
        networkStr,
      );
      const OCTAS_PER_APT = 100_000_000;
      const balanceApt = (balanceOctas / OCTAS_PER_APT).toFixed(8);

      return {
        account_address: accountAddress,
        balance_octas: balanceOctas,
        balance_apt: balanceApt,
      };
    } catch (error) {
      logger.error("Failed to get APT balance:", error);
      throw error;
    }
  }

  async getFABalance(
    accountAddress: string,
    metadataAddress: string,
    networkStr?: string,
  ): Promise<AptosFABalanceResponse> {
    try {
      logger.info(
        `Getting FA balance for ${accountAddress}, metadata: ${metadataAddress}`,
      );

      const balance = await aptosConnection.getAccountFABalance(
        accountAddress,
        metadataAddress,
        networkStr,
      );

      return {
        account_address: accountAddress,
        metadata_address: metadataAddress,
        balance,
      };
    } catch (error) {
      logger.error("Failed to get FA balance:", error);
      throw error;
    }
  }

  getWalletInfo(walletId: string): AptosAccountInfo | null {
    const wallet = aptosDatabase.getWallet(walletId);
    if (!wallet) return null;

    return {
      wallet_id: wallet.wallet_id,
      account_address: wallet.account_address,
      public_key: wallet.public_key,
      user_id: wallet.user_id,
      label: wallet.label,
      signing_scheme: wallet.signing_scheme,
      network: wallet.network,
      created_at: wallet.created_at,
      last_used: wallet.last_used,
      is_primary: wallet.is_primary,
    };
  }

  getUserWallets(userId: string): AptosAccountInfo[] {
    return aptosDatabase.getUserWallets(userId).map((w) => ({
      wallet_id: w.wallet_id,
      account_address: w.account_address,
      public_key: w.public_key,
      user_id: w.user_id,
      label: w.label,
      signing_scheme: w.signing_scheme,
      network: w.network,
      created_at: w.created_at,
      last_used: w.last_used,
      is_primary: w.is_primary,
    }));
  }

  getCachedAccount(walletId: string): CachedAptosWallet | null {
    return this.walletCache.get(walletId) ?? null;
  }

  exportPrivateKey(walletId: string, userId: string): string {
    const wallet = aptosDatabase.getWallet(walletId);
    if (!wallet) {
      throw new Error("Wallet not found");
    }
    if (wallet.user_id !== userId) {
      throw new Error("Unauthorized: wallet does not belong to user");
    }

    return encryptionService.decrypt(
      wallet.encrypted_private_key,
      wallet.encryption_iv,
      wallet.encryption_tag,
    );
  }
}

export const aptosAccountManager = new AptosAccountManager();
