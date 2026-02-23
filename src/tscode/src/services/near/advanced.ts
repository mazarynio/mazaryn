import {
  Account,
  KeyPair,
  MultiKeySigner,
  actions,
  teraToGas,
  nearToYocto,
  keyToImplicitAddress,
} from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { NEAR } from "near-api-js/tokens";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  CreateMetaTransactionRequest,
  CreateMetaTransactionResponse,
  RelayMetaTransactionRequest,
  RelayMetaTransactionResponse,
  CreateImplicitAccountResponse,
  FundImplicitAccountRequest,
  FundImplicitAccountResponse,
  AddMultipleKeysRequest,
  AddMultipleKeysResponse,
  MultiKeyTransferRequest,
  MultiKeyTransferResponse,
  BatchAction,
} from "../../core/near-types.js";

export class NearAdvancedManager {
  private buildSignerAccount(walletId: string): {
    account: Account;
    accountId: string;
    privateKeyString: KeyPairString;
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

    return { account, accountId: wallet.account_id, privateKeyString };
  }

  private buildActions(rawActions: BatchAction[]): any[] {
    return rawActions.map((a) => {
      switch (a.type) {
        case "transfer": {
          if (!a.transfer) throw new Error("transfer field required");
          return actions.transfer(nearToYocto(a.transfer.amount_near));
        }
        case "functionCall": {
          if (!a.function_call) throw new Error("function_call field required");
          return actions.functionCall(
            a.function_call.method_name,
            a.function_call.args ?? {},
            teraToGas(a.function_call.gas_tera ?? "30"),
            a.function_call.deposit_near
              ? nearToYocto(a.function_call.deposit_near)
              : 0n,
          );
        }
        case "addFullAccessKey": {
          if (!a.add_full_access_key)
            throw new Error("add_full_access_key field required");
          return actions.addFullAccessKey(a.add_full_access_key.public_key);
        }
        case "addFunctionCallKey": {
          if (!a.add_function_call_key)
            throw new Error("add_function_call_key field required");
          const fck = a.add_function_call_key;
          return actions.addFunctionCallAccessKey(
            fck.public_key,
            fck.contract_id,
            fck.method_names ?? [],
            fck.allowance_near ? nearToYocto(fck.allowance_near) : undefined,
          );
        }
        case "deleteKey": {
          if (!a.delete_key) throw new Error("delete_key field required");
          return actions.deleteKey(a.delete_key.public_key);
        }
        case "stake": {
          if (!a.stake) throw new Error("stake field required");
          return actions.stake(
            nearToYocto(a.stake.amount_near),
            a.stake.public_key,
          );
        }
        default:
          throw new Error(`Unsupported action type: ${(a as any).type}`);
      }
    });
  }

  private extractTxHash(result: any): string {
    if (result?.transaction?.hash) return result.transaction.hash;
    if (typeof result === "string") return result;
    return JSON.stringify(result);
  }

  async createMetaTransaction(
    req: CreateMetaTransactionRequest,
    userId: string,
  ): Promise<CreateMetaTransactionResponse> {
    const wallet = nearDatabase.getWallet(req.from_wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Creating meta-transaction from ${wallet.account_id} to ${req.receiver_id}`,
    );

    const { account, accountId } = this.buildSignerAccount(req.from_wallet_id);
    const builtActions = this.buildActions(req.actions);

    const result = await account.createSignedMetaTransaction({
      receiverId: req.receiver_id,
      actions: builtActions,
      blockHeightTtl: req.block_height_ttl ?? 100,
    });

    nearDatabase.updateWalletLastUsed(req.from_wallet_id);

    logger.info(`Meta-transaction created for ${accountId}`);

    return {
      from_account_id: accountId,
      receiver_id: req.receiver_id,
      signed_delegate: result.signedDelegate,
      timestamp: Date.now(),
    };
  }

  async relayMetaTransaction(
    req: RelayMetaTransactionRequest,
    userId: string,
  ): Promise<RelayMetaTransactionResponse> {
    const wallet = nearDatabase.getWallet(req.relayer_wallet_id);
    if (!wallet) throw new Error("Relayer wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(`Relaying meta-transaction using account ${wallet.account_id}`);

    const { account, accountId } = this.buildSignerAccount(
      req.relayer_wallet_id,
    );

    const result = await account.relayMetaTransaction(
      req.signed_delegate as any,
    );

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(req.relayer_wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.relayer_wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: "meta-transaction-relay",
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "meta_transaction_relay",
    });

    logger.info(`Meta-transaction relayed: ${txHash}`);

    return {
      transaction_hash: txHash,
      relayer_account_id: accountId,
      status: "confirmed",
      timestamp: Date.now(),
      raw: result,
    };
  }

  async createImplicitAccount(
    userId: string,
  ): Promise<CreateImplicitAccountResponse> {
    logger.info(`Creating implicit account for user: ${userId}`);

    const newKeyPair = KeyPair.fromRandom("ED25519");
    const implicitAccountId = keyToImplicitAddress(newKeyPair.getPublicKey());
    const publicKey = newKeyPair.getPublicKey().toString();

    const privateKeyHex = Buffer.from(newKeyPair.toString()).toString("hex");
    const encrypted = encryptionService.encrypt(privateKeyHex);
    const walletId = `near_wallet_${Date.now()}_${Math.random().toString(36).substring(7)}`;
    const createdAt = Date.now();

    nearDatabase.createWallet({
      wallet_id: walletId,
      user_id: userId,
      account_id: implicitAccountId,
      public_key: publicKey,
      encrypted_private_key: encrypted.encryptedData,
      encryption_iv: encrypted.iv,
      encryption_tag: encrypted.tag,
      account_type: "implicit",
      network: config.nearNetwork,
      created_at: createdAt,
    });

    logger.info(`Implicit account created and stored: ${implicitAccountId}`);

    return {
      account_id: implicitAccountId,
      wallet_id: walletId,
      user_id: userId,
      public_key: publicKey,
      account_type: "implicit",
      network: config.nearNetwork,
      created_at: createdAt,
      note: "This account does not exist on-chain until it receives at least 1 yoctoNEAR. Use /near/advanced/fund-implicit to activate it.",
    };
  }

  async fundImplicitAccount(
    req: FundImplicitAccountRequest,
    userId: string,
  ): Promise<FundImplicitAccountResponse> {
    const wallet = nearDatabase.getWallet(req.from_wallet_id);
    if (!wallet) throw new Error("Funding wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    const amountYocto = req.amount_yocto ?? "1";

    logger.info(
      `Funding implicit account ${req.implicit_account_id} with ${amountYocto} yocto from ${wallet.account_id}`,
    );

    const { account, accountId } = this.buildSignerAccount(req.from_wallet_id);

    const result = await account.transfer({
      token: NEAR,
      amount: BigInt(amountYocto),
      receiverId: req.implicit_account_id,
    });

    const txHash = this.extractTxHash(result);

    nearDatabase.updateWalletLastUsed(req.from_wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.from_wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: req.implicit_account_id,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: 1,
      tx_type: "implicit_fund",
      amount_near: amountYocto,
    });

    logger.info(`Implicit account funded: ${txHash}`);

    return {
      transaction_hash: txHash,
      from_account_id: accountId,
      implicit_account_id: req.implicit_account_id,
      amount_yocto: amountYocto,
      status: "confirmed",
      timestamp: Date.now(),
    };
  }

  async addMultipleKeys(
    req: AddMultipleKeysRequest,
    userId: string,
  ): Promise<AddMultipleKeysResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    if (req.key_count < 1 || req.key_count > 50) {
      throw new Error("key_count must be between 1 and 50");
    }

    logger.info(`Adding ${req.key_count} keys to account ${wallet.account_id}`);

    const { account, accountId } = this.buildSignerAccount(req.wallet_id);

    const newKeyPairs: KeyPair[] = [];
    const txActions: any[] = [];

    for (let i = 0; i < req.key_count; i++) {
      const kp = KeyPair.fromRandom("ed25519");
      newKeyPairs.push(kp);
      txActions.push(actions.addFullAccessKey(kp.getPublicKey()));
    }

    const result = await account.signAndSendTransaction({
      receiverId: accountId,
      actions: txActions,
    });

    const txHash = this.extractTxHash(result);

    const labelPrefix = req.label_prefix ?? "key";
    const keyDetails: Array<{
      public_key: string;
      private_key: string;
      label: string;
    }> = [];

    for (let i = 0; i < newKeyPairs.length; i++) {
      const kp = newKeyPairs[i];
      const label = `${labelPrefix}-${i + 1}`;

      nearDatabase.createAccessKey({
        wallet_id: req.wallet_id,
        user_id: userId,
        account_id: accountId,
        public_key: kp.getPublicKey().toString(),
        private_key_plain: kp.toString(),
        key_type: "full_access",
        label,
      });

      keyDetails.push({
        public_key: kp.getPublicKey().toString(),
        private_key: kp.toString(),
        label,
      });
    }

    nearDatabase.updateWalletLastUsed(req.wallet_id);
    nearDatabase.createTransactionRecord({
      wallet_id: req.wallet_id,
      user_id: userId,
      from_account_id: accountId,
      receiver_id: accountId,
      transaction_hash: txHash,
      status: "confirmed",
      actions_count: req.key_count,
      tx_type: "add_multiple_keys",
    });

    logger.info(`${req.key_count} keys added to ${accountId}: ${txHash}`);

    return {
      wallet_id: req.wallet_id,
      account_id: accountId,
      keys_added: req.key_count,
      transaction_hash: txHash,
      keys: keyDetails,
      timestamp: Date.now(),
    };
  }

  async multiKeyTransfer(
    req: MultiKeyTransferRequest,
    userId: string,
  ): Promise<MultiKeyTransferResponse> {
    const wallet = nearDatabase.getWallet(req.wallet_id);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    logger.info(
      `Multi-key transfer: ${req.transfers.length} transfers from ${wallet.account_id}`,
    );

    const storedKeys = nearDatabase.getAccessKeys(req.wallet_id);
    if (storedKeys.length === 0) {
      throw new Error(
        "No stored access keys found. Use /near/advanced/add-multiple-keys first.",
      );
    }

    const keyPairs: KeyPair[] = storedKeys.map((k) => {
      const privateKeyHex = encryptionService.decrypt(
        k.encrypted_private_key,
        k.encryption_iv,
        k.encryption_tag,
      );
      const privateKeyString = Buffer.from(
        privateKeyHex,
        "hex",
      ).toString() as KeyPairString;
      return KeyPair.fromString(privateKeyString);
    });

    const provider = nearConnection.getProvider();
    const multiKeySigner = new MultiKeySigner(keyPairs);
    const multiAccount = new Account(
      wallet.account_id,
      provider,
      multiKeySigner,
    );

    const transferPromises = req.transfers.map(async (t) => {
      try {
        const result = await multiAccount.transfer({
          token: NEAR,
          amount: NEAR.toUnits(t.amount_near),
          receiverId: t.receiver_id,
        });

        const txHash = this.extractTxHash(result);

        nearDatabase.createTransactionRecord({
          wallet_id: req.wallet_id,
          user_id: userId,
          from_account_id: wallet.account_id,
          receiver_id: t.receiver_id,
          transaction_hash: txHash,
          status: "confirmed",
          actions_count: 1,
          tx_type: "multi_key_transfer",
          amount_near: t.amount_near,
        });

        return {
          receiver_id: t.receiver_id,
          transaction_hash: txHash,
          status: "confirmed" as const,
        };
      } catch (err) {
        logger.error(`Multi-key transfer to ${t.receiver_id} failed:`, err);
        return {
          receiver_id: t.receiver_id,
          status: "failed" as const,
          error: err instanceof Error ? err.message : "Unknown error",
        };
      }
    });

    const results = await Promise.all(transferPromises);

    nearDatabase.updateWalletLastUsed(req.wallet_id);

    const successful = results.filter((r) => r.status === "confirmed").length;
    const failed = results.filter((r) => r.status === "failed").length;

    logger.info(
      `Multi-key transfer complete: ${successful} succeeded, ${failed} failed`,
    );

    return {
      wallet_id: req.wallet_id,
      account_id: wallet.account_id,
      total_transfers: req.transfers.length,
      successful,
      failed,
      results,
      timestamp: Date.now(),
    };
  }
}

export const nearAdvancedManager = new NearAdvancedManager();
