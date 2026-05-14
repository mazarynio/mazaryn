import { aptosConnection } from "./connection.js";
import { aptosAccountManager } from "./account.js";
import { aptosDatabase } from "../database/aptos-database.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  AptosTransferRequest,
  AptosTransferResponse,
  AptosTransactionResponse,
  AptosGasEstimateResponse,
} from "../../core/aptos-types.js";

export class AptosTransactionManager {
  async transferAPT(req: AptosTransferRequest): Promise<AptosTransferResponse> {
    try {
      logger.info(
        `Transferring ${req.amount_octas} octas from ${req.sender_address} to ${req.recipient_address}`,
      );

      const wallet = aptosDatabase.getWalletByAddress(req.sender_address);
      if (!wallet) {
        throw new Error("Sender wallet not found");
      }

      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Sender account not loaded in cache");
      }

      const client = aptosConnection.getClient(req.network);

      const transaction = await client.transaction.build.simple({
        sender: cached.account.accountAddress,
        data: {
          function: "0x1::aptos_account::transfer",
          functionArguments: [req.recipient_address, req.amount_octas],
        },
        options: {
          gasUnitPrice: req.options?.gas_unit_price,
          maxGasAmount: req.options?.max_gas_amount,
        },
      });

      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });

      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });

      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);

      aptosDatabase.recordTransaction({
        wallet_id: wallet.wallet_id,
        user_id: wallet.user_id,
        from_address: req.sender_address,
        to_address: req.recipient_address,
        transaction_hash: pendingTxn.hash,
        tx_type: "apt_transfer",
        amount_octas: req.amount_octas,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        network: req.network ?? config.aptosNetwork,
      });

      logger.info(`APT transfer committed: ${pendingTxn.hash}`);

      return {
        transaction_hash: pendingTxn.hash,
        sender_address: req.sender_address,
        recipient_address: req.recipient_address,
        amount_octas: req.amount_octas,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to transfer APT:", error);
      throw error;
    }
  }

  async getTransaction(
    transactionHash: string,
    networkStr?: string,
  ): Promise<AptosTransactionResponse> {
    try {
      logger.info("Getting transaction:", transactionHash);

      const txn = await aptosConnection.getTransaction(
        transactionHash,
        networkStr,
      );

      return {
        transaction_hash: (txn as any).hash ?? transactionHash,
        version: String((txn as any).version ?? ""),
        sender: (txn as any).sender ?? "",
        sequence_number: Number((txn as any).sequence_number ?? 0),
        gas_used: Number((txn as any).gas_used ?? 0),
        gas_unit_price: Number((txn as any).gas_unit_price ?? 0),
        success: (txn as any).success ?? true,
        vm_status: (txn as any).vm_status ?? "",
        timestamp: Number((txn as any).timestamp ?? Date.now()),
        function_called: (txn as any).payload?.function,
      };
    } catch (error) {
      logger.error("Failed to get transaction:", error);
      throw error;
    }
  }

  async getAccountTransactions(
    accountAddress: string,
    networkStr?: string,
  ): Promise<AptosTransactionResponse[]> {
    try {
      logger.info("Getting account transactions for:", accountAddress);

      const txns = await aptosConnection.getAccountTransactions(
        accountAddress,
        networkStr,
      );

      return txns.map((txn: any) => ({
        transaction_hash: txn.hash,
        version: String(txn.version ?? ""),
        sender: txn.sender ?? "",
        sequence_number: Number(txn.sequence_number ?? 0),
        gas_used: Number(txn.gas_used ?? 0),
        gas_unit_price: Number(txn.gas_unit_price ?? 0),
        success: txn.success ?? true,
        vm_status: txn.vm_status ?? "",
        timestamp: Number(txn.timestamp ?? 0),
        function_called: txn.payload?.function,
      }));
    } catch (error) {
      logger.error("Failed to get account transactions:", error);
      throw error;
    }
  }

  async getLocalTransactionHistory(
    walletId: string,
    userId: string,
    limit: number = 20,
    offset: number = 0,
  ) {
    try {
      logger.info(`Getting local transaction history for wallet: ${walletId}`);

      const wallet = aptosDatabase.getWallet(walletId);
      if (!wallet) {
        throw new Error("Wallet not found");
      }
      if (wallet.user_id !== userId) {
        throw new Error("Unauthorized: wallet does not belong to user");
      }

      const transactions = aptosDatabase.getWalletTransactions(
        walletId,
        limit,
        offset,
      );
      const total = aptosDatabase.countWalletTransactions(walletId);

      return { transactions, total, limit, offset };
    } catch (error) {
      logger.error("Failed to get local transaction history:", error);
      throw error;
    }
  }

  async estimateGas(networkStr?: string): Promise<AptosGasEstimateResponse> {
    try {
      logger.info("Estimating gas price");

      const estimate = await aptosConnection.estimateGasPrice(networkStr);

      return {
        gas_unit_price: Number(estimate.gas_estimate ?? 0),
        prioritized_gas_unit_price: Number(
          estimate.prioritized_gas_estimate ?? 0,
        ),
        deprioritized_gas_unit_price: Number(
          estimate.deprioritized_gas_estimate ?? 0,
        ),
      };
    } catch (error) {
      logger.error("Failed to estimate gas:", error);
      throw error;
    }
  }
}

export const aptosTransactionManager = new AptosTransactionManager();
