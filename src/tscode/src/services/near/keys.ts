import { Account, KeyPair, KeyPairSigner } from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { NEAR } from "near-api-js/tokens";
import { generateSeedPhrase, parseSeedPhrase } from "near-api-js/seed-phrase";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  GetAccessKeysResponse,
  AddFullAccessKeyRequest,
  AddFullAccessKeyResponse,
  AddFunctionCallKeyRequest,
  AddFunctionCallKeyResponse,
  DeleteKeyRequest,
  DeleteKeyResponse,
  GenerateSeedPhraseResponse,
  ImportFromSeedPhraseRequest,
  CreateNearAccountResponse,
} from "../../core/near-types.js";

export class NearKeyManager {
  private buildSignerAccount(walletId: string): {
    account: Account;
    accountId: string;
  } {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) {
      throw new Error("Wallet not found");
    }

    const privateKeyHex = encryptionService.decrypt(
      wallet.encrypted_private_key,
      wallet.encryption_iv,
      wallet.encryption_tag,
    );

    const privateKeyString = Buffer.from(
      privateKeyHex,
      "hex",
    ).toString() as KeyPairString;

    const provider = nearConnection.getProvider();
    const account = new Account(wallet.account_id, provider, privateKeyString);

    return { account, accountId: wallet.account_id };
  }

  async getAccessKeys(
    walletId: string,
    userId: string,
  ): Promise<GetAccessKeysResponse> {
    try {
      const wallet = nearDatabase.getWallet(walletId);
      if (!wallet) {
        throw new Error("Wallet not found");
      }
      if (wallet.user_id !== userId) {
        throw new Error("Unauthorized");
      }

      logger.info(`Getting access keys for account: ${wallet.account_id}`);

      const provider = nearConnection.getProvider();
      const account = new Account(wallet.account_id, provider);
      const result = await account.getAccessKeyList();

      return {
        account_id: wallet.account_id,
        wallet_id: walletId,
        keys: result.keys as any[],
        total: result.keys.length,
        block_hash: result.block_hash,
        block_height: result.block_height,
      };
    } catch (error) {
      logger.error("Failed to get access keys:", error);
      throw error;
    }
  }

  async getAccessKeysByAccountId(
    accountId: string,
  ): Promise<GetAccessKeysResponse> {
    try {
      logger.info(`Getting access keys for public account: ${accountId}`);

      const provider = nearConnection.getProvider();
      const account = new Account(accountId, provider);
      const result = await account.getAccessKeyList();

      return {
        account_id: accountId,
        keys: result.keys as any[],
        total: result.keys.length,
        block_hash: result.block_hash,
        block_height: result.block_height,
      };
    } catch (error) {
      logger.error("Failed to get access keys by account id:", error);
      throw error;
    }
  }

  async addFullAccessKey(
    req: AddFullAccessKeyRequest,
    userId: string,
  ): Promise<AddFullAccessKeyResponse> {
    try {
      const wallet = nearDatabase.getWallet(req.wallet_id);
      if (!wallet) {
        throw new Error("Wallet not found");
      }
      if (wallet.user_id !== userId) {
        throw new Error("Unauthorized");
      }

      logger.info(`Adding full access key to account: ${wallet.account_id}`);

      const { account, accountId } = this.buildSignerAccount(req.wallet_id);
      const newKeyPair = KeyPair.fromRandom("ed25519");
      const newPublicKey = newKeyPair.getPublicKey().toString();

      await account.addFullAccessKey(newPublicKey);

      logger.info(`Full access key added to ${accountId}: ${newPublicKey}`);

      nearDatabase.createAccessKey({
        wallet_id: req.wallet_id,
        user_id: userId,
        account_id: accountId,
        public_key: newPublicKey,
        private_key_plain: newKeyPair.toString(),
        key_type: "full_access",
        label: req.label,
      });

      return {
        wallet_id: req.wallet_id,
        account_id: accountId,
        new_public_key: newPublicKey,
        new_private_key: newKeyPair.toString(),
        label: req.label,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to add full access key:", error);
      throw error;
    }
  }

  async addFunctionCallKey(
    req: AddFunctionCallKeyRequest,
    userId: string,
  ): Promise<AddFunctionCallKeyResponse> {
    try {
      const wallet = nearDatabase.getWallet(req.wallet_id);
      if (!wallet) {
        throw new Error("Wallet not found");
      }
      if (wallet.user_id !== userId) {
        throw new Error("Unauthorized");
      }

      logger.info(
        `Adding function call key to account: ${wallet.account_id} for contract: ${req.contract_id}`,
      );

      const { account, accountId } = this.buildSignerAccount(req.wallet_id);
      const newKeyPair = KeyPair.fromRandom("ed25519");
      const newPublicKey = newKeyPair.getPublicKey().toString();

      const allowance = req.allowance_near
        ? NEAR.toUnits(req.allowance_near)
        : undefined;

      await account.addFunctionCallAccessKey({
        publicKey: newPublicKey,
        contractId: req.contract_id,
        methodNames: req.method_names ?? [],
        allowance,
      });

      logger.info(`Function call key added to ${accountId}: ${newPublicKey}`);

      nearDatabase.createAccessKey({
        wallet_id: req.wallet_id,
        user_id: userId,
        account_id: accountId,
        public_key: newPublicKey,
        private_key_plain: newKeyPair.toString(),
        key_type: "function_call",
        contract_id: req.contract_id,
        method_names: req.method_names,
        allowance_near: req.allowance_near,
        label: req.label,
      });

      return {
        wallet_id: req.wallet_id,
        account_id: accountId,
        new_public_key: newPublicKey,
        new_private_key: newKeyPair.toString(),
        contract_id: req.contract_id,
        method_names: req.method_names ?? [],
        allowance_near: req.allowance_near,
        label: req.label,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to add function call key:", error);
      throw error;
    }
  }

  async deleteKey(
    req: DeleteKeyRequest,
    userId: string,
  ): Promise<DeleteKeyResponse> {
    try {
      const wallet = nearDatabase.getWallet(req.wallet_id);
      if (!wallet) {
        throw new Error("Wallet not found");
      }
      if (wallet.user_id !== userId) {
        throw new Error("Unauthorized");
      }

      logger.info(
        `Deleting key ${req.public_key} from account: ${wallet.account_id}`,
      );

      const { account, accountId } = this.buildSignerAccount(req.wallet_id);
      await account.deleteKey(req.public_key);

      nearDatabase.deactivateAccessKey(req.wallet_id, req.public_key);

      logger.info(`Key ${req.public_key} deleted from ${accountId}`);

      return {
        wallet_id: req.wallet_id,
        account_id: accountId,
        deleted_public_key: req.public_key,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to delete key:", error);
      throw error;
    }
  }

  generateSeedPhrase(): GenerateSeedPhraseResponse {
    try {
      logger.info("Generating new seed phrase");
      const { seedPhrase, keyPair } = generateSeedPhrase();

      return {
        seed_phrase: seedPhrase,
        public_key: keyPair.getPublicKey().toString(),
        private_key: keyPair.toString(),
      };
    } catch (error) {
      logger.error("Failed to generate seed phrase:", error);
      throw error;
    }
  }

  async importFromSeedPhrase(
    req: ImportFromSeedPhraseRequest,
  ): Promise<CreateNearAccountResponse> {
    try {
      logger.info(
        `Importing NEAR account ${req.account_id} from seed phrase for user: ${req.user_id}`,
      );

      if (nearDatabase.walletExists(req.account_id)) {
        throw new Error("Account already imported");
      }

      const keyPair = parseSeedPhrase(req.seed_phrase);
      const privateKeyString = keyPair.toString() as KeyPairString;
      const publicKey = keyPair.getPublicKey().toString();

      const privateKeyHex = Buffer.from(privateKeyString).toString("hex");
      const encrypted = encryptionService.encrypt(privateKeyHex);
      const walletId = `near_wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
      const createdAt = Date.now();

      const suffix = config.nearNetwork === "mainnet" ? ".near" : ".testnet";
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

      logger.info(`Account imported from seed phrase: ${walletId}`);

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
      logger.error("Failed to import account from seed phrase:", error);
      throw error;
    }
  }

  async getStoredKeys(walletId: string, userId: string): Promise<any[]> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) {
      throw new Error("Wallet not found");
    }
    if (wallet.user_id !== userId) {
      throw new Error("Unauthorized");
    }
    return nearDatabase.getAccessKeys(walletId);
  }
}

export const nearKeyManager = new NearKeyManager();
