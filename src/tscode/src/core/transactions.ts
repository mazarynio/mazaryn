import {
  Account,
  KeyPairSigner,
  actions,
  teraToGas,
  nearToYocto,
} from "near-api-js";
import type { KeyPairString } from "near-api-js";
import { nearConnection } from "./connection.js";
import { nearDatabase } from "../database/near-database.js";
import { encryptionService } from "../security/encryption.js";
import { logger } from "../../core/logger.js";
import type {
  SignAndSendTransactionRequest,
  SignAndSendTransactionResponse,
  CallFunctionRequest,
  CallFunctionResponse,
  ViewFunctionRequest,
  ViewFunctionResponse,
  CreateAndSignTransactionRequest,
  CreateAndSignTransactionResponse,
  BatchAction,
} from "../../core/near-types.js";

export class NearTransactionManager {
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

  async signAndSendTransaction(
    req: SignAndSendTransactionRequest,
    userId: string,
  ): Promise<SignAndSendTransactionResponse> {
    try {
      const wallet = nearDatabase.getWallet(req.from_wallet_id);
      if (!wallet) throw new Error("Wallet not found");
      if (wallet.user_id !== userId) throw new Error("Unauthorized");

      logger.info(
        `Signing and sending ${req.actions.length} action(s) to ${req.receiver_id} from ${wallet.account_id}`,
      );

      const { account, accountId } = this.buildSignerAccount(
        req.from_wallet_id,
      );
      const builtActions = this.buildActions(req.actions);

      const result = await account.signAndSendTransaction({
        receiverId: req.receiver_id,
        actions: builtActions,
        waitUntil: (req.wait_until as any) ?? "EXECUTED",
      });

      const txHash = this.extractTxHash(result);

      nearDatabase.updateWalletLastUsed(req.from_wallet_id);
      nearDatabase.createTransactionRecord({
        wallet_id: req.from_wallet_id,
        user_id: userId,
        from_account_id: accountId,
        receiver_id: req.receiver_id,
        transaction_hash: txHash,
        status: "confirmed",
        actions_count: req.actions.length,
        tx_type: "batch",
      });

      logger.info(`Batch transaction confirmed: ${txHash}`);

      return {
        transaction_hash: txHash,
        from_account_id: accountId,
        receiver_id: req.receiver_id,
        actions_count: req.actions.length,
        status: "confirmed",
        timestamp: Date.now(),
        raw: result,
      };
    } catch (error) {
      logger.error("signAndSendTransaction failed:", error);
      throw error;
    }
  }

  async callFunction(
    req: CallFunctionRequest,
    userId: string,
  ): Promise<CallFunctionResponse> {
    try {
      const wallet = nearDatabase.getWallet(req.from_wallet_id);
      if (!wallet) throw new Error("Wallet not found");
      if (wallet.user_id !== userId) throw new Error("Unauthorized");

      logger.info(
        `Calling ${req.method_name} on ${req.contract_id} from ${wallet.account_id}`,
      );

      const { account, accountId } = this.buildSignerAccount(
        req.from_wallet_id,
      );

      const result = await account.callFunction({
        contractId: req.contract_id,
        methodName: req.method_name,
        args: req.args ?? {},
        gas: teraToGas(req.gas_tera ?? "30"),
        deposit: req.deposit_near ? nearToYocto(req.deposit_near) : 0n,
        waitUntil: (req.wait_until as any) ?? "EXECUTED",
      });

      const txHash = this.extractTxHash(result);

      nearDatabase.updateWalletLastUsed(req.from_wallet_id);
      nearDatabase.createTransactionRecord({
        wallet_id: req.from_wallet_id,
        user_id: userId,
        from_account_id: accountId,
        receiver_id: req.contract_id,
        transaction_hash: txHash,
        status: "confirmed",
        actions_count: 1,
        tx_type: "function_call",
        contract_id: req.contract_id,
        method_name: req.method_name,
        gas_tera: req.gas_tera,
      });

      logger.info(`Contract call confirmed: ${txHash}`);

      return {
        transaction_hash: txHash,
        from_account_id: accountId,
        contract_id: req.contract_id,
        method_name: req.method_name,
        status: "confirmed",
        timestamp: Date.now(),
        raw: result,
      };
    } catch (error) {
      logger.error("callFunction failed:", error);
      throw error;
    }
  }

  async viewFunction(req: ViewFunctionRequest): Promise<ViewFunctionResponse> {
    try {
      logger.info(`View call: ${req.method_name} on ${req.contract_id}`);

      const provider = nearConnection.getProvider();
      const result = await (provider as any).callFunction({
        contractId: req.contract_id,
        method: req.method_name,
        args: req.args ?? {},
      });

      return {
        contract_id: req.contract_id,
        method_name: req.method_name,
        result,
      };
    } catch (error) {
      logger.error("viewFunction failed:", error);
      throw error;
    }
  }

  async createAndSignTransaction(
    req: CreateAndSignTransactionRequest,
    userId: string,
  ): Promise<CreateAndSignTransactionResponse> {
    try {
      const wallet = nearDatabase.getWallet(req.from_wallet_id);
      if (!wallet) throw new Error("Wallet not found");
      if (wallet.user_id !== userId) throw new Error("Unauthorized");

      logger.info(
        `Creating and signing transaction for ${wallet.account_id} → ${req.receiver_id}`,
      );

      const { account, accountId, privateKeyString } = this.buildSignerAccount(
        req.from_wallet_id,
      );

      const signer = KeyPairSigner.fromSecretKey(privateKeyString);
      const signerPublicKey = await signer.getPublicKey();
      const builtActions = this.buildActions(req.actions);

      const transaction = await account.createTransaction({
        receiverId: req.receiver_id,
        actions: builtActions,
        publicKey: signerPublicKey,
      });

      const signResult = await signer.signTransaction(transaction);

      return {
        from_account_id: accountId,
        receiver_id: req.receiver_id,
        signed_transaction: signResult.signedTransaction,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("createAndSignTransaction failed:", error);
      throw error;
    }
  }

  async sendSignedTransaction(signedTransaction: any): Promise<any> {
    try {
      logger.info("Sending pre-signed transaction");
      const result = await nearConnection.sendTransaction(signedTransaction);
      logger.info("Pre-signed transaction sent:", this.extractTxHash(result));
      return result;
    } catch (error) {
      logger.error("sendSignedTransaction failed:", error);
      throw error;
    }
  }

  async getTransactionStatus(txHash: string, accountId: string): Promise<any> {
    try {
      logger.info(`Getting status for tx: ${txHash}`);
      return await nearConnection.getTransaction(txHash, accountId);
    } catch (error) {
      logger.error("getTransactionStatus failed:", error);
      throw error;
    }
  }

  async getUserTransactions(
    walletId: string,
    userId: string,
    limit: number,
    offset: number,
  ): Promise<any> {
    const wallet = nearDatabase.getWallet(walletId);
    if (!wallet) throw new Error("Wallet not found");
    if (wallet.user_id !== userId) throw new Error("Unauthorized");

    const records = nearDatabase.getTransactionRecords(walletId, limit, offset);
    const total = nearDatabase.countTransactionRecords(walletId);

    return {
      wallet_id: walletId,
      account_id: wallet.account_id,
      total,
      limit,
      offset,
      transactions: records,
    };
  }
}

export const nearTransactionManager = new NearTransactionManager();
