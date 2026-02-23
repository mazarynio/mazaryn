import { Account, KeyPair, nearToYocto } from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  CreateNamedAccountRequest,
  CreateSubAccountRequest,
  ImportNearAccountRequest,
  DeleteNearAccountRequest,
  CreateNearAccountResponse,
  DeleteNearAccountResponse,
  NearAccountInfo,
  NearBalanceResponse,
  NearAccountStateResponse,
} from "../../core/near-types.js";

interface CachedNearWallet {
  keyPair: KeyPair;
  accountId: string;
}

export class NearAccountManager {
  private walletCache: Map<string, CachedNearWallet>;

  constructor() {
    this.walletCache = new Map();
    this.loadWalletsFromDatabase();
  }

  private async loadWalletsFromDatabase(): Promise<void> {
    try {
      const wallets = nearDatabase.getAllWallets();
      logger.info(`Loading ${wallets.length} NEAR wallets from database`);

      for (const wallet of wallets) {
        try {
          const privateKeyHex = encryptionService.decrypt(
            wallet.encrypted_private_key,
            wallet.encryption_iv,
            wallet.encryption_tag,
          );

          const privateKeyString = Buffer.from(
            privateKeyHex,
            "hex",
          ).toString() as KeyPairString;

          const keyPair = KeyPair.fromString(privateKeyString);

          this.walletCache.set(wallet.wallet_id, {
            keyPair,
            accountId: wallet.account_id,
          });
        } catch (error) {
          logger.error(
            `Failed to load NEAR wallet ${wallet.wallet_id}:`,
            error,
          );
        }
      }

      logger.info(
        `Successfully loaded ${this.walletCache.size} NEAR wallets into cache`,
      );
    } catch (error) {
      logger.error("Failed to load NEAR wallets from database:", error);
    }
  }

  private generateWalletId(): string {
    return `near_wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
  }

  private getNetworkSuffix(): string {
    return config.nearNetwork === "mainnet" ? ".near" : ".testnet";
  }

  private getTopLevelAccount(): string {
    return config.nearNetwork === "mainnet" ? "near" : "testnet";
  }

  private buildSignerAccount(cached: CachedNearWallet): Account {
    const provider = nearConnection.getProvider();
    return new Account(
      cached.accountId,
      provider,
      cached.keyPair.toString() as KeyPairString,
    );
  }

  private encryptAndStore(privateKeyString: string): {
    encryptedData: string;
    iv: string;
    tag: string;
  } {
    const privateKeyHex = Buffer.from(privateKeyString).toString("hex");
    return encryptionService.encrypt(privateKeyHex);
  }

  async createNamedAccount(
    req: CreateNamedAccountRequest,
  ): Promise<CreateNearAccountResponse> {
    try {
      logger.info(
        `Creating named NEAR account "${req.account_name}" for user: ${req.user_id}`,
      );

      const signerCached = this.walletCache.get(req.signer_wallet_id);
      if (!signerCached) {
        throw new Error("Signer wallet not found or not loaded");
      }

      const suffix = this.getNetworkSuffix();
      const newAccountId = `${req.account_name}${suffix}`;

      const alreadyExists = await nearConnection.accountExists(newAccountId);
      if (alreadyExists) {
        throw new Error(`Account ${newAccountId} already exists on-chain`);
      }

      const newKeyPair = KeyPair.fromRandom("ed25519");
      const newPublicKey = newKeyPair.getPublicKey().toString();
      const initialBalance = req.initial_balance_near ?? "0";

      const signerAccount = this.buildSignerAccount(signerCached);

      if (req.method === "contract") {
        const topLevelAccount = this.getTopLevelAccount();
        await signerAccount.callFunction({
          contractId: topLevelAccount,
          methodName: "create_account",
          args: {
            new_account_id: newAccountId,
            new_public_key: newPublicKey,
          },
        });
        logger.info(
          `Named account created via contract on-chain: ${newAccountId}`,
        );
      } else {
        await signerAccount.createAccount({
          newAccountId,
          publicKey: newPublicKey,
          nearToTransfer: nearToYocto(initialBalance),
        });
        logger.info(`Named account created directly on-chain: ${newAccountId}`);
      }

      const encrypted = this.encryptAndStore(newKeyPair.toString());
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      nearDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        account_id: newAccountId,
        public_key: newPublicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        account_name: req.account_name,
        account_type: "named",
        network: config.nearNetwork,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, {
        keyPair: newKeyPair,
        accountId: newAccountId,
      });

      logger.info(`Named NEAR wallet persisted: ${walletId}`);

      return {
        account_id: newAccountId,
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: newPublicKey,
        account_type: "named",
        network: config.nearNetwork,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to create named NEAR account:", error);
      throw error;
    }
  }

  async createSubAccount(
    req: CreateSubAccountRequest,
  ): Promise<CreateNearAccountResponse> {
    try {
      logger.info(
        `Creating sub-account "${req.prefix}" for user: ${req.user_id}`,
      );

      const signerCached = this.walletCache.get(req.signer_wallet_id);
      if (!signerCached) {
        throw new Error("Signer wallet not found or not loaded");
      }

      const newKeyPair = KeyPair.fromRandom("ed25519");
      const newPublicKey = newKeyPair.getPublicKey().toString();
      const initialBalance = req.initial_balance_near ?? "0";
      const newAccountId = `${req.prefix}.${signerCached.accountId}`;

      const alreadyExists = await nearConnection.accountExists(newAccountId);
      if (alreadyExists) {
        throw new Error(`Sub-account ${newAccountId} already exists on-chain`);
      }

      const signerAccount = this.buildSignerAccount(signerCached);

      await signerAccount.createSubAccount({
        accountOrPrefix: req.prefix,
        publicKey: newPublicKey,
        nearToTransfer: nearToYocto(initialBalance),
      });

      logger.info(`Sub-account created on-chain: ${newAccountId}`);

      const encrypted = this.encryptAndStore(newKeyPair.toString());
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      nearDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        account_id: newAccountId,
        public_key: newPublicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        account_name: req.prefix,
        account_type: "subaccount",
        parent_account_id: signerCached.accountId,
        network: config.nearNetwork,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, {
        keyPair: newKeyPair,
        accountId: newAccountId,
      });

      logger.info(`Sub-account NEAR wallet persisted: ${walletId}`);

      return {
        account_id: newAccountId,
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: newPublicKey,
        account_type: "subaccount",
        parent_account_id: signerCached.accountId,
        network: config.nearNetwork,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to create NEAR sub-account:", error);
      throw error;
    }
  }

  async deleteAccount(
    req: DeleteNearAccountRequest,
  ): Promise<DeleteNearAccountResponse> {
    try {
      logger.info(
        `Deleting NEAR account for wallet: ${req.wallet_id}, user: ${req.user_id}`,
      );

      const targetCached = this.walletCache.get(req.wallet_id);
      if (!targetCached) {
        throw new Error("Target wallet not found or not loaded");
      }

      const beneficiaryCached = this.walletCache.get(req.beneficiary_wallet_id);
      if (!beneficiaryCached) {
        throw new Error("Beneficiary wallet not found or not loaded");
      }

      if (targetCached.accountId === beneficiaryCached.accountId) {
        throw new Error(
          "Target account and beneficiary account cannot be the same",
        );
      }

      const provider = nearConnection.getProvider();
      const accountToDelete = new Account(
        targetCached.accountId,
        provider,
        targetCached.keyPair.toString() as KeyPairString,
      );

      await accountToDelete.deleteAccount(beneficiaryCached.accountId);

      logger.info(
        `Account deleted on-chain: ${targetCached.accountId}, funds sent to: ${beneficiaryCached.accountId}`,
      );

      nearDatabase.markWalletDeleted(
        req.wallet_id,
        req.user_id,
        beneficiaryCached.accountId,
      );

      this.walletCache.delete(req.wallet_id);

      const timestamp = Date.now();

      logger.info(`NEAR wallet marked deleted in DB: ${req.wallet_id}`);

      return {
        success: true,
        deleted_account_id: targetCached.accountId,
        beneficiary_account_id: beneficiaryCached.accountId,
        timestamp,
      };
    } catch (error) {
      logger.error("Failed to delete NEAR account:", error);
      throw error;
    }
  }

  async importAccount(
    req: ImportNearAccountRequest,
  ): Promise<CreateNearAccountResponse> {
    try {
      logger.info("Importing NEAR account for user:", req.user_id);

      if (nearDatabase.walletExists(req.account_id)) {
        throw new Error("Account already imported");
      }

      const keyPair = KeyPair.fromString(req.private_key as KeyPairString);
      const publicKey = keyPair.getPublicKey().toString();

      const encrypted = this.encryptAndStore(req.private_key);
      const walletId = this.generateWalletId();
      const createdAt = Date.now();

      const suffix = this.getNetworkSuffix();
      const accountType = req.account_id.endsWith(suffix)
        ? "named"
        : "implicit";

      nearDatabase.createWallet({
        wallet_id: walletId,
        user_id: req.user_id,
        account_id: req.account_id,
        public_key: publicKey,
        encrypted_private_key: encrypted.encryptedData,
        encryption_iv: encrypted.iv,
        encryption_tag: encrypted.tag,
        account_name: req.account_name,
        account_type: accountType as "named" | "implicit",
        network: config.nearNetwork,
        created_at: createdAt,
      });

      this.walletCache.set(walletId, {
        keyPair,
        accountId: req.account_id,
      });

      logger.info(`NEAR wallet imported and persisted: ${walletId}`);

      return {
        account_id: req.account_id,
        wallet_id: walletId,
        user_id: req.user_id,
        public_key: publicKey,
        account_type: accountType as "named" | "implicit",
        network: config.nearNetwork,
        created_at: createdAt,
      };
    } catch (error) {
      logger.error("Failed to import NEAR account:", error);
      throw error;
    }
  }

  async getBalance(accountId: string): Promise<NearBalanceResponse> {
    try {
      logger.info("Getting NEAR balance for:", accountId);

      const balanceYocto = await nearConnection.getBalance(accountId);
      const YOCTO_PER_NEAR = BigInt("1000000000000000000000000");
      const balanceNear = (BigInt(balanceYocto) / YOCTO_PER_NEAR).toString();

      return {
        account_id: accountId,
        balance_yocto: balanceYocto,
        balance_near: balanceNear,
      };
    } catch (error) {
      logger.error("Failed to get NEAR balance:", error);
      throw error;
    }
  }

  async getAccountState(accountId: string): Promise<NearAccountStateResponse> {
    try {
      logger.info("Getting NEAR account state for:", accountId);
      const state = await nearConnection.getAccountState(accountId);

      return {
        account_id: accountId,
        amount: state.amount,
        locked: state.locked,
        code_hash: state.code_hash,
        storage_usage: state.storage_usage,
        storage_paid_at: state.storage_paid_at,
      };
    } catch (error) {
      logger.error("Failed to get NEAR account state:", error);
      throw error;
    }
  }

  async getWalletInfo(walletId: string): Promise<NearAccountInfo | null> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) {
      return null;
    }

    return {
      wallet_id: wallet.wallet_id,
      account_id: wallet.account_id,
      public_key: wallet.public_key,
      user_id: wallet.user_id,
      account_name: wallet.account_name,
      account_type: wallet.account_type,
      parent_account_id: wallet.parent_account_id,
      network: wallet.network,
      created_at: wallet.created_at,
    };
  }

  async getUserWallets(userId: string): Promise<NearAccountInfo[]> {
    const wallets = nearDatabase.getUserWallets(userId);
    return wallets.map((w) => ({
      wallet_id: w.wallet_id,
      account_id: w.account_id,
      public_key: w.public_key,
      user_id: w.user_id,
      account_name: w.account_name,
      account_type: w.account_type,
      parent_account_id: w.parent_account_id,
      network: w.network,
      created_at: w.created_at,
    }));
  }

  async getSubAccounts(parentWalletId: string): Promise<NearAccountInfo[]> {
    const parentWallet = nearDatabase.getWallet(parentWalletId);
    if (!parentWallet) {
      throw new Error("Parent wallet not found");
    }

    const subAccounts = nearDatabase.getSubAccounts(parentWallet.account_id);
    return subAccounts.map((w) => ({
      wallet_id: w.wallet_id,
      account_id: w.account_id,
      public_key: w.public_key,
      user_id: w.user_id,
      account_name: w.account_name,
      account_type: w.account_type,
      parent_account_id: w.parent_account_id,
      network: w.network,
      created_at: w.created_at,
    }));
  }

  getWalletSigner(
    walletId: string,
  ): { keyPair: KeyPair; accountId: string } | null {
    const cached = this.walletCache.get(walletId);
    if (!cached) {
      return null;
    }
    return {
      keyPair: cached.keyPair,
      accountId: cached.accountId,
    };
  }

  async exportPrivateKey(walletId: string): Promise<string> {
    try {
      const wallet = nearDatabase.getWallet(walletId);
      if (!wallet) {
        throw new Error("Wallet not found");
      }

      const privateKeyHex = encryptionService.decrypt(
        wallet.encrypted_private_key,
        wallet.encryption_iv,
        wallet.encryption_tag,
      );

      return Buffer.from(privateKeyHex, "hex").toString();
    } catch (error) {
      logger.error("Failed to export NEAR private key:", error);
      throw error;
    }
  }
}

export const nearAccountManager = new NearAccountManager();
