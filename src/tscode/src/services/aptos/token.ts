import { AccountAddress } from "@aptos-labs/ts-sdk";
import { aptosConnection } from "./connection.js";
import { aptosAccountManager } from "./account.js";
import { aptosDatabase } from "../database/aptos-database.js";
import { logger } from "../../core/logger.js";
import { config } from "../../config/index.js";
import type {
  AptosGetCoinBalanceRequest,
  AptosGetCoinBalanceResponse,
  AptosGetFAMetadataRequest,
  AptosGetFAMetadataResponse,
  AptosGetFABalancesRequest,
  AptosGetFABalancesResponse,
  AptosTransferCoinRequest,
  AptosTransferFungibleAssetRequest,
  AptosMintCoinRequest,
  AptosViewFunctionRequest,
  AptosViewFunctionResponse,
  AptosGetAccountCoinsRequest,
  AptosGetAccountCoinsResponse,
  AptosTransferResponse,
} from "../../core/aptos-types.js";

export class AptosTokenManager {
  async getCoinBalance(
    req: AptosGetCoinBalanceRequest,
  ): Promise<AptosGetCoinBalanceResponse> {
    try {
      logger.info(
        `Getting coin balance for ${req.account_address}, coin: ${req.coin_type}`,
      );
      const client = aptosConnection.getClient(req.network);
      const balance = await client.getAccountCoinAmount({
        accountAddress: req.account_address,
        coinType: req.coin_type as `${string}::${string}::${string}`,
      });
      return {
        account_address: req.account_address,
        coin_type: req.coin_type,
        balance,
      };
    } catch (error) {
      logger.error("Failed to get coin balance:", error);
      throw error;
    }
  }

  async getFAMetadata(
    req: AptosGetFAMetadataRequest,
  ): Promise<AptosGetFAMetadataResponse> {
    try {
      logger.info(`Getting FA metadata for asset: ${req.asset_type}`);
      const client = aptosConnection.getClient(req.network);
      const data = await client.getFungibleAssetMetadataByAssetType({
        assetType: req.asset_type as `${string}::${string}::${string}`,
      });
      if (!data) {
        throw new Error(
          `Fungible asset metadata not found for: ${req.asset_type}`,
        );
      }
      return {
        asset_type: (data as any).asset_type ?? req.asset_type,
        name: data.name,
        symbol: data.symbol,
        decimals: data.decimals,
        icon_uri: (data as any).icon_uri ?? undefined,
        project_uri: (data as any).project_uri ?? undefined,
        creator_address: data.creator_address,
      };
    } catch (error) {
      logger.error("Failed to get FA metadata:", error);
      throw error;
    }
  }

  async getFABalances(
    req: AptosGetFABalancesRequest,
  ): Promise<AptosGetFABalancesResponse> {
    try {
      logger.info(`Getting all FA balances for: ${req.account_address}`);
      const client = aptosConnection.getClient(req.network);
      const balances = await client.getCurrentFungibleAssetBalances({
        options: {
          where: {
            owner_address: { _eq: req.account_address },
          },
        },
      });
      return {
        account_address: req.account_address,
        balances: balances.map((b: any) => ({
          asset_type: b.asset_type ?? "",
          amount: b.amount ?? 0,
          is_primary: b.is_primary ?? false,
        })),
      };
    } catch (error) {
      logger.error("Failed to get FA balances:", error);
      throw error;
    }
  }

  async getAccountCoins(
    req: AptosGetAccountCoinsRequest,
  ): Promise<AptosGetAccountCoinsResponse> {
    try {
      logger.info(`Getting account coins data for: ${req.account_address}`);
      const client = aptosConnection.getClient(req.network);
      const coinsData = await client.getAccountCoinsData({
        accountAddress: req.account_address,
      });
      return {
        account_address: req.account_address,
        total: coinsData.length,
        coins: coinsData.map((c: any) => ({
          asset_type: c.asset_type ?? "",
          amount: c.amount ?? 0,
          name: c.metadata?.name ?? undefined,
          symbol: c.metadata?.symbol ?? undefined,
          decimals: c.metadata?.decimals ?? undefined,
        })),
      };
    } catch (error) {
      logger.error("Failed to get account coins:", error);
      throw error;
    }
  }

  async transferCoin(
    req: AptosTransferCoinRequest,
  ): Promise<AptosTransferResponse> {
    try {
      logger.info(
        `Transferring ${req.amount} of ${req.coin_type} from ${req.sender_address} to ${req.recipient_address}`,
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
      const transaction = await client.transferCoinTransaction({
        sender: cached.account.accountAddress,
        recipient: AccountAddress.fromString(req.recipient_address),
        amount: req.amount,
        coinType: req.coin_type as `${string}::${string}::${string}`,
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
      logger.info(`Coin transfer committed: ${pendingTxn.hash}`);
      return {
        transaction_hash: pendingTxn.hash,
        sender_address: req.sender_address,
        recipient_address: req.recipient_address,
        amount_octas: req.amount,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to transfer coin:", error);
      throw error;
    }
  }

  async transferFungibleAsset(
    req: AptosTransferFungibleAssetRequest,
  ): Promise<AptosTransferResponse> {
    try {
      logger.info(
        `Transferring FA ${req.metadata_address} from ${req.sender_address} to ${req.recipient_address}`,
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
      const transaction = await client.transferFungibleAsset({
        sender: cached.account,
        fungibleAssetMetadataAddress: AccountAddress.fromString(
          req.metadata_address,
        ),
        recipient: AccountAddress.fromString(req.recipient_address),
        amount: req.amount,
      });
      const pendingTxn = await client.signAndSubmitTransaction({
        signer: cached.account,
        transaction,
      });
      const executedTxn = await client.waitForTransaction({
        transactionHash: pendingTxn.hash,
      });
      aptosDatabase.updateWalletLastUsed(wallet.wallet_id);
      logger.info(`FA transfer committed: ${pendingTxn.hash}`);
      return {
        transaction_hash: pendingTxn.hash,
        sender_address: req.sender_address,
        recipient_address: req.recipient_address,
        amount_octas: req.amount,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to transfer fungible asset:", error);
      throw error;
    }
  }

  async mintCoin(req: AptosMintCoinRequest): Promise<AptosTransferResponse> {
    try {
      logger.info(
        `Minting ${req.amount} of ${req.coin_module_address}::${req.coin_module_name} to ${req.recipient_address}`,
      );
      const wallet = aptosDatabase.getWalletByAddress(req.admin_address);
      if (!wallet) {
        throw new Error("Admin wallet not found");
      }
      const cached = aptosAccountManager.getCachedAccount(wallet.wallet_id);
      if (!cached) {
        throw new Error("Admin account not loaded in cache");
      }
      const client = aptosConnection.getClient(req.network);
      const transaction = await client.transaction.build.simple({
        sender: cached.account.accountAddress,
        data: {
          function: `${req.coin_module_address}::${req.coin_module_name}::mint`,
          functionArguments: [
            AccountAddress.fromString(req.recipient_address),
            req.amount,
          ],
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
      logger.info(`Coin mint committed: ${pendingTxn.hash}`);
      return {
        transaction_hash: pendingTxn.hash,
        sender_address: req.admin_address,
        recipient_address: req.recipient_address,
        amount_octas: req.amount,
        gas_used: Number((executedTxn as any).gas_used ?? 0),
        gas_unit_price: Number((executedTxn as any).gas_unit_price ?? 0),
        version: String((executedTxn as any).version ?? ""),
        vm_status: (executedTxn as any).vm_status ?? "Executed successfully",
        success: (executedTxn as any).success ?? true,
        timestamp: Date.now(),
      };
    } catch (error) {
      logger.error("Failed to mint coin:", error);
      throw error;
    }
  }

  async viewFunction(
    req: AptosViewFunctionRequest,
  ): Promise<AptosViewFunctionResponse> {
    try {
      logger.info(`Calling view function: ${req.function}`);
      const client = aptosConnection.getClient(req.network);
      const result = await client.view({
        payload: {
          function: req.function as `${string}::${string}::${string}`,
          typeArguments: (req.type_arguments ??
            []) as `${string}::${string}::${string}`[],
          functionArguments: req.function_arguments ?? [],
        },
      });
      return { result };
    } catch (error) {
      logger.error("Failed to call view function:", error);
      throw error;
    }
  }

  async getFungibleAssetActivities(
    asset_type: string,
    limit: number = 10,
    networkStr?: string,
  ): Promise<any[]> {
    try {
      logger.info(`Getting FA activities for asset: ${asset_type}`);
      const client = aptosConnection.getClient(networkStr);
      const data = await client.getFungibleAssetActivities({
        options: {
          limit,
          where: {
            asset_type: { _eq: asset_type },
          },
        },
      });
      return data;
    } catch (error) {
      logger.error("Failed to get FA activities:", error);
      throw error;
    }
  }

  async getAllFungibleAssetMetadata(
    creatorAddress: string,
    networkStr?: string,
  ): Promise<any[]> {
    try {
      logger.info(`Getting all FA metadata for creator: ${creatorAddress}`);
      const client = aptosConnection.getClient(networkStr);
      const data = await client.getFungibleAssetMetadata({
        options: {
          where: {
            creator_address: { _eq: creatorAddress },
          },
        },
      });
      return data;
    } catch (error) {
      logger.error("Failed to get all FA metadata:", error);
      throw error;
    }
  }
}

export const aptosTokenManager = new AptosTokenManager();
