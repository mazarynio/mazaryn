import { Account } from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { createHash } from "crypto";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import type {
  DeployContractRequest,
  DeployContractResponse,
  DeployGlobalContractRequest,
  DeployGlobalContractResponse,
  UseGlobalContractRequest,
  UseGlobalContractResponse,
} from "../../core/near-types.js";

export class NearContractManager {
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

  private extractTxHash(result: any): string {
    if (result?.transaction?.hash) return result.transaction.hash;
    if (typeof result === "string") return result;
    return JSON.stringify(result);
  }

  async deployContract(
    req: DeployContractRequest,
    userId: string,
  ): Promise<DeployContractResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Deploying contract to account: ${wallet.account_id}`);

    const wasmBytes = new Uint8Array(Buffer.from(req.wasm_base64, "base64"));
    const { account, accountId } = this.buildSignerAccount(req.wallet_id);

    const result = await account.deployContract(wasmBytes);
    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(req.wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "deploy_contract",
    });

    logger.info(`Contract deployed to ${accountId}: ${txHash}`);

    return {
      wallet_id: req.wallet_id,
      account_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async deployGlobalContract(
    req: DeployGlobalContractRequest,
    userId: string,
  ): Promise<DeployGlobalContractResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Deploying global contract from account: ${wallet.account_id}, mode: ${req.deploy_mode}`,
    );

    const wasmBytes = new Uint8Array(Buffer.from(req.wasm_base64, "base64"));
    const { account, accountId } = this.buildSignerAccount(req.wallet_id);

    const result = await account.deployGlobalContract(
      wasmBytes,
      req.deploy_mode,
    );
    const txHash = this.extractTxHash(result);

    let codeHash: string | undefined;
    if (req.deploy_mode === "codeHash") {
      const hashBytes = createHash("sha256").update(wasmBytes).digest();
      codeHash = hashBytes.toString("base64");
    }

    nearDatabase.updateWalletLastUsed(req.wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "deploy_global_contract",
    });

    logger.info(`Global contract deployed from ${accountId}: ${txHash}`);

    return {
      wallet_id: req.wallet_id,
      account_id: accountId,
      transaction_hash: txHash,
      deploy_mode: req.deploy_mode,
      code_hash: codeHash,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async useGlobalContract(
    req: UseGlobalContractRequest,
    userId: string,
  ): Promise<UseGlobalContractResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Using global contract on account: ${wallet.account_id}, type: ${req.identifier_type}`,
    );

    const { account, accountId } = this.buildSignerAccount(req.wallet_id);

    let contractIdentifier:
      | { accountId: string }
      | { codeHash: string | Uint8Array };

    if (req.identifier_type === "accountId") {
      if (!req.account_id)
        throw new Error(
          "account_id is required when identifier_type is accountId",
        );
      contractIdentifier = { accountId: req.account_id };
    } else {
      if (!req.code_hash)
        throw new Error(
          "code_hash is required when identifier_type is codeHash",
        );
      contractIdentifier = {
        codeHash: Uint8Array.from(Buffer.from(req.code_hash, "base64")),
      };
    }

    const result = await account.useGlobalContract(contractIdentifier);
    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(req.wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "use_global_contract",
    });

    logger.info(`Global contract linked to ${accountId}: ${txHash}`);

    return {
      wallet_id: req.wallet_id,
      account_id: accountId,
      transaction_hash: txHash,
      identifier_type: req.identifier_type,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async getContractCode(walletId: string, userId: string): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Getting contract code for account: ${wallet.account_id}`);

    const provider = nearConnection.getProvider();
    const account = new Account(wallet.account_id, provider);
    const code = await account.getContractCode();

    return {
      wallet_id: walletId,
      account_id: wallet.account_id,
      code_hash: code.hash,
      code_size_bytes: code.code.length,
    };
  }

  async getContractCodeByAccountId(accountId: string): Promise<any> {
    logger.info(`Getting contract code for public account: ${accountId}`);

    const provider = nearConnection.getProvider();
    const account = new Account(accountId, provider);
    const code = await account.getContractCode();

    return {
      account_id: accountId,
      code_hash: code.hash,
      code_size_bytes: code.code.length,
    };
  }

  async getContractState(
    walletId: string,
    userId: string,
    prefix?: string,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Getting contract state for account: ${wallet.account_id}`);

    const provider = nearConnection.getProvider();
    const account = new Account(wallet.account_id, provider);
    const state = await account.getContractState(prefix);

    return {
      wallet_id: walletId,
      account_id: wallet.account_id,
      block_hash: state.block_hash,
      block_height: state.block_height,
      values: state.values,
    };
  }
}

export const nearContractManager = new NearContractManager();
